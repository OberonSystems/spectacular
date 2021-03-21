(ns spectacular.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clojure.set :refer [subset?]]
            ;;
            [spectacular.utils :refer [keyword->label]]))

;;;

(defn -set!
  [cache k m]
  ;; Maintain an ordinal of when a map was added to a cache so that we
  ;; can return values in the order they were added.
  ;;
  ;; Very helpful when debugging or doing code gen.
  (swap! cache #(assoc % k
                       (assoc m ::ordinal (or (get   @cache [k ::ordinal])
                                              (count @cache)))))
  ;; It gets noisy in the repl when always returning the updated cache
  ;; so just return nil.
  nil)

(defn -get
  [cache k kind]
  (if-let [record (get @cache k)]
    record
    (throw (ex-info (str kind " not found.") k))))

(defn -get-in
  [cache k ks kind]
  (if-let [val (get-in @cache k ks)]
    val
    (throw (ex-info (str kind " not found.") k))))

(defn -exists?
  [cache k]
  (contains? @cache k))

;;;

(defonce +scalars+ (atom {}))

(defmacro register-scalar
  [k pred & {:as info}]
  (let [k#    k
        pred# pred
        info# info]
    `(do
       (s/def ~k ~pred#)
       (-set! +scalars+ ~k ~info#))))

(defn scalar?
  [k]
  (-exists? +scalars+ k))

(defn get-scalar
  ([k]      (-get +scalars+ k "Scalar"))
  ([k & ks] (-> (get-scalar k) (get-in ks))))

(defn get-scalar-label
  [k]
  (get-scalar k ::label))

(defn get-scalar-description
  [k]
  (get-scalar k ::description))

;;; A bit of extra sugar on top of scalars

(defmacro register-enum
  [k values & {:as info}]
  `(let [values# ~values]
     (s/def ~k (set values#))
     (-set! +scalars+ ~k (assoc ~info ::values values#))))

(defn get-enum-values
  [k]
  (or (get-scalar k ::values)
      (throw (ex-info "Scalar is not an enum." k))))

(defn get-enum-labels
  [k]
  (let [labels (get-scalar k ::labels)]
    (->> (get-enum-values k)
         (mapv (fn [value]
                 [value (or (get labels value)
                            (keyword->label value))])))))

;;;

(defonce +fields+ (atom {}))

(defmacro register-field
  [k sk & {:as info}]
  ;; Fixme: do some validation on k and sk being namespaced keys
  (when-not (scalar? sk)
    (throw (ex-info "Cannot register field for unregistered scalar." {:field-key k :scalar-key sk})))
  `(do
     (s/def ~k (s/get-spec ~sk))
     (-set! +fields+ ~k (assoc ~info ::scalar-key ~sk))))

(defn field?
  [k]
  (-exists? +fields+ k))

(defn get-field
  ([k]      (-get +fields+ k "Field"))
  ([k & ks] (-> (get-field k) (get-in ks))))

(defn get-field-label
  [k]
  (get-field k ::label))

(defn get-field-description
  [k]
  (get-field k ::description))

(defn get-field-scalar
  [k]
  (-> (get-field k ::scalar-key)
      get-scalar))

(defn get-fields
  [ks]
  (->> ks
       (mapv (fn [fk]
               (let [field  (get-field        fk)
                     scalar (get-field-scalar fk)]
                 (merge {::field-key fk}
                        scalar
                        field))))
       seq))

;;;

(defonce +entities+ (atom {}))

(defmacro register-entity
  [k field-ks & {::keys [identity optional] :as info}]
  ;; FIXME: clean this up, we don't need all the gensyms if we check
  ;; the macro input types.
  (let [k#         k
        field-ks#  field-ks
        info#      info
        ;;
        field-ks?  (set field-ks)
        identity?  (set identity)
        optional?  (set optional)
        ;;
        error      (cond
                     (and identity (not (subset? identity? field-ks?))) "Identity must be a subset of fields."
                     (and optional (not (subset? optional? field-ks?))) "Optional must be a subset of fields."
                     (and optional      (subset? optional? identity?))  "Optional cannot be a subset of identity.")
        _          (when error
                     (throw (ex-info error {:k          k
                                            :field-keys field-ks
                                            :identity   identity
                                            :optional   optional})))
        required#  (->> field-ks (remove optional?) vec)
        optional#  optional]
    (when-let [unregistered (->> field-ks (remove field?) seq)]
      (throw (ex-info "Cannot register entity with unregistered field."
                      {:k k :unregistered unregistered})))
    `(do
       (s/def ~k (s/keys :req ~required# :opt ~optional#))
       (-set! +entities+ ~k (assoc ~info# ::field-keys ~field-ks#)))))

(defn get-entity
  ([k]      (-get +entities+ k "Entity"))
  ([k & ks] (-> (get-entity k) (get-in ks))))

(defn get-entity-identity
  [k]
  (get-entity k ::identity))

(defn get-entity-label
  [k]
  (get-entity k ::label))

(defn get-entity-description
  [k]
  (get-entity k ::description))

(defn get-entity-fields
  [k]
  (let [optional? (-> (get-entity k ::optional) set)]
    (->> (get-entity k ::field-keys)
         get-fields
         (map (fn [{::keys [field-key] :as record}]
                (assoc record ::optional? (-> (optional? field-key) boolean)))))))
