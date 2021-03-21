(ns spectacular.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clojure.set :refer [subset? intersection]]
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
    (throw (ex-info (str kind " not found.")
                    {:k k :kind kind}))))

(defn -get-in
  [cache k ks kind]
  (if-let [val (get-in @cache k ks)]
    val
    (throw (ex-info (str kind " not found.")
                    {:k k :ks ks :kind kind}k))))

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
  [k field-keys & {::keys [identity-keys optional-keys] :as info}]
  ;; FIXME: Add spec checks for all field keys to be non-empty sequences of
  ;; namespaced keys to this funciont.
  (let [field-key?    (set field-keys)
        identity-key? (set identity-keys)
        optional-key? (set optional-keys)
        ;;
        required-keys (some->> field-keys (remove optional-key?) seq vec)
        content-keys  (some->> field-keys (remove identity-key?) seq vec)
        ;;
        content-key?  (set content-keys)
        ;;
        error (cond
                (and identity-keys (not (subset? identity-key? field-key?))) "Identity keys must be a subset of field keys."
                (and optional-keys (not (subset? optional-key? field-key?))) "Optional keys must be a subset of field keys."
                (-> (intersection identity-key? optional-key?) empty? not)   "Identity and Optional keys cannot intersect.")]
    (when error
      (throw (ex-info error {:k          k
                             :field-keys      field-keys
                             :identity-keys   identity-keys
                             :optional-keys   optional-keys})))
    (when-let [unregistered (->> field-keys (remove field?) seq)]
      (throw (ex-info "Cannot register an entity with an unregistered fields."
                      {:k k :unregistered unregistered})))
    `(do
       (s/def ~k (s/keys :req ~required-keys
                         :opt ~optional-keys))
       (-set! +entities+ ~k (assoc ~info
                                   ::field-keys   ~field-keys
                                   ::content-keys ~content-keys
                                   ;;
                                   ::field-key?     ~field-key?
                                   ::identity-key?  ~identity-key?
                                   ::optional-key?  ~optional-key?
                                   ::content-key?   ~content-key?)))))

(defn get-entity
  ([k]      (-get +entities+ k "Entity"))
  ([k & ks] (-> (get-entity k) (get-in ks))))

(defn get-entity-label
  [k]
  (get-entity k ::label))

(defn get-entity-description
  [k]
  (get-entity k ::description))

(defn get-field-keys
  [k]
  (get-entity k ::field-keys))

(defn get-identity-keys
  [k]
  (get-entity k ::identity-keys))

(defn get-optional-keys
  [k]
  (get-entity k ::optional-keys))

(defn get-content-keys
  [k]
  (get-entity k ::content-keys))

(defn get-entity-fields
  ([k]
   (get-entity-fields k (get-entity k ::field-keys)))
  ([k fields]
   (let [optional-key? (get-entity k ::optional-key?)
         overrides     (get-entity k ::fields)]
     (->> fields
          get-fields
          (map (fn [{::keys [field-key] :as record}]
                 (merge record
                        (get overrides field-key)
                        (when (optional-key? field-key) {::optional? true}))))))))

(defn get-identity-fields
  [k]
  (->> k get-identity-keys (get-entity-fields k)))

(defn get-content-fields
  [k]
  (->> k get-content-keys (get-entity-fields k)))
