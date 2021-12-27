(ns spectacular.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clojure.set :refer [subset? intersection union difference]]
            ;;
            [spectacular.utils :refer [nsk? keyword->label]]))

;;;

(defonce +registry+ (atom {}))

;;;

(defn -set
  [k kind m]
  (when-not (nsk? k)
    (throw (ex-info "Can only register Namespaced Keywords" {:k k})))
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

(defn enum?
  [k]
  (and (scalar? k)
       (-get k ::enum?)))

(defn attr?
  [k]
  (= (-get k ::kind) ::attribute))

(defn entity?
  [k]
  (= (-get k ::kind) ::entity))

(defn get-scalar
  [k]
  (when (scalar? k)
    (-get k)))

(defn get-attribute
  [k]
  (when (attr? k)
    (-get k)))

(defn get-entity
  [k]
  (when (entity? k)
    (-get k)))

;;;

(defmacro scalar
  [k pred & {:as info}]
  `(do
     (s/def ~k ~pred)
     (-set  ~k ::scalar ~info)))

(defmacro enum
  [k values & {:as info}]
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
  (when-not (scalar? sk)
    (throw (ex-info "Attribute must be associated with a registered scalar." {:attribute-key k :scalar-key sk})))
  `(do
     (s/def ~k (s/get-spec ~sk))
     (-set ~k ::attribute (assoc ~info ::scalar-key ~sk))))

(defmacro entity
  [k attribute-keys & {:keys [::identity-keys ::required-keys]
                       :as info}]
  (let [attribute-ks (some-> attribute-keys seq vec)
        identity-ks  (some-> identity-keys  seq vec)
        required-ks  (some-> required-keys  seq vec)
        ;;
        attribute-set (set attribute-ks)
        identity-set  (set identity-ks)
        required-set  (set required-keys)
        ;;
        sp-required-ks (some-> (union      identity-set  required-set)   seq vec)
        sp-optional-ks (some-> (difference attribute-set sp-required-ks) seq vec)]
    (when-let [[error info] (cond
                              (empty? attribute-ks)
                              ["Cannot register and entity without attributes."]

                              (not (every? nsk? attribute-ks))
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
      (throw (ex-info error (merge {:k              k
                                    :attribute-keys attribute-keys
                                    :identity-keys  identity-keys
                                    :required-keys  required-keys}
                                   info))))
    `(do
       (s/def ~k (s/keys :req ~sp-required-ks :opt ~sp-optional-ks))
       (-set  ~k ::entity (assoc ~info
                                 ::attribute-keys ~attribute-ks
                                 ::identity-keys  ~identity-ks
                                 ::required-keys  ~required-ks)))))

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
