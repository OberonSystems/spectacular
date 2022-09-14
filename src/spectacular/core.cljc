(ns spectacular.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clojure.set :refer [subset? intersection union difference]]
            ;;
            [spectacular.utils :refer [keyword->label ns-keyword->keyword]]))

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

(def !scalar? (complement scalar?))

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

(defn throw-when-not-attr?
  [k msg]
  (when-not (attr? k)
    (throw (ex-info msg {:k k}))))

(defn attr-set?
  [k]
  (throw-when-not-attr? k "attr-set? can only be called on an attribute")
  (-get k ::set?))

(defn attr-vector?
  [k]
  (throw-when-not-attr? k "attr-vector? can only be called on an attribute")
  (-get k ::vector?))

(defn attr-optional?
  [k]
  (throw-when-not-attr? k "attr-optional? can only be called on an attribute")
  (-get k ::optional?))

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
    (-get k ::attribute-type)))

;;; --------------------------------------------------------------------------------

(defn label
  [k]
  (or (-get k ::label)
      (keyword->label k)))

(defn values
  [k]
  (-get k ::values))

(defn attribute-type
  [k]
  (-get k ::attribute-type))

(defn description
  [k]
  (-get k ::description))

(defn attribute-keys
  [k]
  (-get k ::attribute-keys))

(defn identity-keys
  [k]
  (-get k ::identity-keys))

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
  [k t & {is-set?    ::set?
          is-vector? ::vector?
          optional?  ::optional?
          ;;
          count     ::count
          min-count ::min-count
          max-count ::max-count
          :as info}]
  (throw-when-registered ::attribute k)
  (when-let [error (cond
                     (not (or (scalar? t) (entity? t)))
                     "Attribute must be associated with a registered scalar or entity."
                     ;;
                     (and is-set? (!scalar? t))
                     "The set? flag can only be used with scalar attributes."
                     ;;
                     (and count (or min-count max-count))
                     "Cannot pass count and min/max-count."
                     ;;
                     (and count (zero? count))
                     "Cannot specify a count of zero. Use optional? instead."

                     (and min-count (zero? min-count))
                     "Cannot specify a min-count of zero. Use optional? instead."
                     ;;
                     (and (or min-count max-count)
                          (not (and min-count max-count)))
                     "Must provide both min/max count or neither.")]
    (throw (ex-info error {:attribute-key  k
                           :attribute-type t
                           ;;
                           :info info})))
  (let [info      (assoc info ::attribute-type t)
        ;; We always insist on non empty lists.
        min-count (when-not count (or min-count 1))
        pred      (cond
                    is-set?    `(s/every ~t :kind set?    :distinct true :count ~count :min-count ~min-count :max-count ~max-count)
                    is-vector? `(s/every ~t :kind vector?                :count ~count :min-count ~min-count :max-count ~max-count)
                    :else t)]
    `(do
       ~(if optional?
          `(s/def ~k (s/nilable ~pred))
          `(s/def ~k ~pred))
       (-set ~k ::attribute ~info))))

(defmacro entity
  [k attribute-keys & {:keys [::identity-keys]
                       :as info}]
  (throw-when-registered ::entity k)
  (let [attribute-ks (some-> attribute-keys seq vec)
        identity-ks  (some-> identity-keys  seq vec)
        ;;
        attribute-set (set attribute-ks)
        identity-set  (set identity-ks)
        ;;
        value-ks (some->> attribute-ks (remove identity-set) seq vec)]
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

                              (some attr-optional? identity-ks)
                              ["Identity Keys cannot be optional?"])]
      (throw (ex-info error {:entity-key     k
                             ;;
                             :attribute-keys attribute-keys
                             :identity-keys  identity-keys
                             ;;
                             :info info})))
    `(do
       (s/def ~k (s/keys :req ~attribute-ks))
       (-set  ~k ::entity (assoc ~info
                                 ::attribute-keys ~attribute-ks
                                 ::identity-keys  ~identity-ks
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
                                         ::identity-keys  ~identity-ks)))))

(defmacro entity-values
  [values-key entity-key & {:as info}]
  (throw-when-registered ::entity-values values-key)
  (let [value-ks  (value-keys entity-key)
        value-set (set value-ks)]
    (when-not value-ks
      (ex-info "Cannot create a values-entity for an entity that does not have any value keys."
               {:values-key values-key
                :entity-key entity-key
                :info       info}))
    `(do
       (s/def ~values-key (s/keys :req ~value-ks))
       (-set  ~values-key ::entity (assoc ~info
                                          ::entity-key     ~entity-key
                                          ::attribute-keys ~value-ks
                                          ;;
                                          ::values?       true
                                          ::value-keys    ~value-ks)))))

;;; --------------------------------------------------------------------------------

(defn map->entity
  [entity-type m & {:keys [extras]}]
  (some->> (concat (attribute-keys entity-type)
                   ;; It's handy to be able to also provide a way for
                   ;; our caller to include some extra attributes that
                   ;; they might be interested in.
                   extras)
           (map (fn [ref-type]
                  (let [value (if (contains? m ref-type)
                                (get m ref-type)
                                ;; We need to test on the presence
                                ;; of the key rather than the value
                                ;; as we don't want a nil or false
                                ;; overridden by a non-namespaced
                                ;; variant.
                                ;;
                                ;; If m doesn't contain the
                                ;; namespaced variant then we can go
                                ;; for a plain keyword.
                                ;;
                                ;; This is often useful for getting
                                ;; things from maps returned from
                                ;; databases.
                                (get m (ns-keyword->keyword ref-type)))]
                    ;; We want to let false values through
                    (when-not (nil? value)
                      [ref-type value]))))
           (remove nil?)
           seq
           (into {})))
