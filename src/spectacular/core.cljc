(ns spectacular.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clojure.set :refer [subset? intersection union difference]]
            ;;
            [spectacular.utils :refer [keyword->label]]))

;;;

(defonce +registry+ (atom {}))

(defn clear!
  [& namespaces]
  (let [cleared (if namespaces
                  (let [clear? (->> namespaces (map name) set)]
                    (->> @+registry+
                         (remove #(->> % first namespace clear?))
                         (into {})))
                  {})]
    (swap! +registry+ (constantly cleared))))

;;;

(defn -set
  [k kind m]
  (when-not (qualified-keyword? k)
    (throw (ex-info "Can only register Namespaced Keywords" {:k k})))
  (swap! +registry+ #(assoc % k (assoc m ::kind kind)))
  k)

(defn -get
  ([k]
   (get @+registry+ k))

  ([k & ks]
   (when-let [v (-get k)]
     (get-in v ks))))

(defn exists?
  [k]
  (contains? @+registry+ k))

(defn scalar?
  [k]
  (= (-get k ::kind) ::scalar))

(defn enum?
  [k]
  (and (scalar? k)
       (-get k ::enum?)))

(defn entity?
  [k]
  (= (-get k ::kind) ::entity))

(defn attr?
  [k]
  (= (-get k ::kind) ::attribute))

(defn get-scalar
  [k]
  (when (scalar? k)
    (-get k)))

(defn get-entity
  [k]
  (when (entity? k)
    (-get k)))

(defn get-attribute
  [k]
  (when (attr? k)
    (-get k)))

(defn get-attribute-type
  [k]
  (when (attr? k)
    (-get k ::scalar-key)))

;;; --------------------------------------------------------------------------------

(defn label
  [k]
  (or (-get k ::label)
      (keyword->label k)))

(defn values
  [k]
  (-get k ::values))

(defn scalar-key
  [k]
  (-get k ::scalar-key))

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

(defn value-keys
  [k]
  (-get k ::value-keys))

;;; --------------------------------------------------------------------------------
;;; Enum specific helpers

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

;;;

(defn throw-when-registered
  [kind k]
  (when (exists? k)
    (throw (ex-info "Key has already been registered."
                    {:kind kind
                     :key  k}))))

(defmacro scalar
  [k pred & {:as info}]
  (throw-when-registered ::scalar k)
  `(do
     (s/def ~k ~pred)
     (-set  ~k ::scalar ~info)))

(defmacro enum
  [k values & {:as info}]
  (throw-when-registered ::enum k)
  (when (empty? values)
    (ex-info "Enums must have values." {:scalar-key k :values values}))
  (let [enums (set values)]
    `(do
       (s/def ~k ~enums)
       (-set  ~k ::scalar (assoc ~info
                                 ::values ~values
                                 ::enum?  true)))))

(defmacro attribute
  [k sk & {:as info}]
  (throw-when-registered ::attribute k)
  (when-not (or (scalar? sk)
                (entity? sk))
    (throw (ex-info "Attribute must be associated with a registered scalar or entity." {:attribute-key k :scalar-key sk})))
  `(do
     (s/def ~k (s/get-spec ~sk))
     (-set ~k ::attribute (assoc ~info ::scalar-key ~sk))))

(defmacro entity
  [k attribute-keys & {:keys [::identity-keys ::required-keys]
                       :as info}]
  (throw-when-registered ::entity k)
  (let [attribute-ks (some-> attribute-keys seq vec)
        identity-ks  (some-> identity-keys  seq vec)
        required-ks  (some-> required-keys  seq vec)
        ;;
        attribute-set (set attribute-ks)
        identity-set  (set identity-ks)
        required-set  (set required-keys)
        ;;
        value-ks (some->> attribute-ks (remove identity-set) seq vec)
        ;;
        sp-required-ks (some-> (union      identity-set  required-set)   seq vec)
        sp-optional-ks (some-> (difference attribute-set sp-required-ks) seq vec)]
    (when-let [[error info] (cond
                              (empty? attribute-ks)
                              ["Cannot register and entity without attributes."]

                              (not (every? qualified-keyword? attribute-ks))
                              ["Cannot register an entity with invalid attributes; all attributes must be namespaced keywords."]

                              (not (every? attr? attribute-ks))
                              ["Cannot register an entity with unregistered attributes."
                               {:unregistered (->> attribute-ks (remove attr?))}]

                              (not (subset? identity-set attribute-set))
                              ["Identity Keys must be a subset of Attribute Keys"]

                              (not (subset? required-set attribute-set))
                              ["Required Keys must be a subset of Attribute Keys"]

                              (not (empty? (intersection identity-set required-set)))
                              ["Identity Keys and Required Keys cannot intersect."])]
      (throw (ex-info error {:entity-key     k
                             ;;
                             :attribute-keys attribute-keys
                             :identity-keys  identity-keys
                             :required-keys  required-keys
                             ;;
                             :info info})))
    `(do
       (s/def ~k (s/keys :req ~sp-required-ks :opt ~sp-optional-ks))
       (-set  ~k ::entity (assoc ~info
                                 ::attribute-keys ~attribute-ks
                                 ::identity-keys  ~identity-ks
                                 ::required-keys  ~required-ks
                                 ::value-keys     ~value-ks)))))

(defmacro entity-token
  [token-key entity-key & {:as info}]
  (throw-when-registered ::entity-token token-key)
  (let [identity-ks (identity-keys entity-key)]
    (when-not identity-ks
      (ex-info "Cannot create an entity-token for an entity that does not have identity keys."
               {:token-key  token-key
                :entity-key entity-key
                :info       info}))
    `(do
       (s/def ~token-key (s/keys :req ~identity-ks))
       (-set  ~token-key ::entity (assoc ~info
                                         ::entity-key     ~entity-key
                                         ::attribute-keys ~identity-ks
                                         ;;
                                         ::token?         true
                                         ::required-keys  ~identity-ks
                                         ::identity-keys  ~identity-ks)))))

(defmacro entity-values
  [values-key entity-key & {:as info}]
  (throw-when-registered ::entity-values values-key)
  (let [value-ks    (value-keys entity-key)
        value-set   (set value-ks)
        ;;
        required-ks (some->> entity-key
                             required-keys
                             (filter #(contains? value-set %))
                             seq
                             vec)]
    (when-not value-ks
      (ex-info "Cannot create a values-entity for an entity that does not have any value keys."
               {:values-key values-key
                :entity-key entity-key
                :info       info}))
    `(do
       (s/def ~values-key (s/keys :opt ~value-ks))
       (-set  ~values-key ::entity (assoc ~info
                                          ::entity-key     ~entity-key
                                          ::attribute-keys ~value-ks
                                          ;;
                                          ::values?       true
                                          ::required-keys ~required-ks
                                          ::value-keys    ~value-ks)))))
