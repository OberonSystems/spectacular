(ns spectacular.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clojure.set :refer [subset? intersection difference]]
            ;;
            [spectacular.utils :refer [keyword->label]]))

;;;

(defonce +registry+ (atom {}))

;;;

(defn -set
  [k kind m]
  ;; Fixme: do some validation on k being a namespaced key
  (swap! +registry+ #(assoc % k (assoc m ::kind kind)))
  k)

(defn -get
  ([& ks]
   (let [[k & ks :as params] (flatten ks)]
     (cond
       (nil? ks) (get    @+registry+ k)
       :else     (get-in @+registry+ params)))))

(defn exists?
  [k]
  (contains? @+registry+ k))

(defn scalar?
  [k]
  (= (-get k ::kind) ::scalar))

(defn attr?
  [k]
  (= (-get k ::kind) ::attribute))

(defn entity?
  [k]
  (= (-get k ::kind) ::entity))

;;;

(defmacro scalar
  [k pred & {:as info}]
  `(do
     (s/def ~k ~pred)
     (-set  ~k ::scalar ~info)))

(defmacro enum
  [k values & {:as info}]
  (when (empty? values)
    (ex-info "Cannot register an enum without values." {:scalar-key k :values values}))
  (let [enums (set values)]
    `(do
       (s/def ~k ~enums)
       (-set  ~k ::scalar (assoc ~info
                                 ::values ~values
                                 ::enum?  true)))))

(defmacro attribute
  [k sk & {:as info}]
  (when-not (exists? sk)
    (throw (ex-info "Cannot register an attribute for unregistered scalar." {:attribute-key k :scalar-key sk})))
  `(do
     (s/def ~k (s/get-spec ~sk))
     (-set ~k ::attribute (assoc ~info ::scalar-key ~sk))))

(defn- ns-keyword?
  [k]
  (and (keyword? k)
       (namespace k)))

(defmacro entity
  [k attribute-keys & {:keys [::identity-keys ::required-keys] :as info}]
  (let [attribute-set (set attribute-keys)
        identity-set  (set identity-keys)
        required-set  (set required-keys)
        ;;
        optional-keys (difference attribute-set required-set)
        ;;
        ;; Reduce to non-empty vectors or nil for simplicity
        [attribute-keys identity-keys required-keys optional-keys]
        (map (fn [ks] (if (empty? ks) nil (vec ks)))
             [attribute-keys identity-keys required-keys optional-keys])]
    (when-let [[error info] (cond
                              (empty? attribute-keys)
                              ["You must provide attribute-keys."]

                              (not (every? ns-keyword? attribute-keys))
                              ["All attribute-keys must be namespaced keywords."]

                              (not (subset? identity-set attribute-set))
                              ["The identity-keys must be a subset of attribute-keys."]

                              (not (subset? required-set attribute-set))
                              ["The required-keys must be a subset of attribute-keys."]

                              (not (every? attr? attribute-keys))
                              ["Cannot register an entity with unregistered attributes."
                               {:unregistered (->> attribute-keys (remove attr?))}])]
      (throw (ex-info error (merge {:k              k
                                    :attribute-keys attribute-keys
                                    :identity-keys  identity-keys
                                    :required-keys  required-keys}
                                   info))))
    `(do
       (s/def ~k (s/keys :req ~required-keys :opt ~optional-keys))
       (-set  ~k ::entity (assoc ~info
                                 ::attribute-keys ~attribute-keys
                                 ::identity-keys  ~identity-keys
                                 ::required-keys  ~required-keys
                                 ::optional-keys  ~optional-keys)))))

;;; --------------------------------------------------------------------------------

(defn label
  [k]
  (or (-get k ::label)
      (keyword->label k)))

(defn values
  [k]
  (-get k ::values))

(defn -enum-values->label
  [k f]
  (let [f (or f keyword->label)]
    (some->> (values k)
             (map (fn [value]
                    [value (f value)]))
             seq
             (into {}))))

(defn labels
  [k]
  (-enum-values->label k (-get k ::labels)))

(defn abbreviations
  [k]
  (-enum-values->label k (-get k ::abbrevs)))

(defn description
  [k]
  (-get k ::description))

(defn attribute-keys
  [k]
  (-get k ::attribute-keys))

(defn identity-keys
  [k]
  (-get k ::identity-keys))

(defn required-keys
  [k]
  (-get k ::required-keys))
