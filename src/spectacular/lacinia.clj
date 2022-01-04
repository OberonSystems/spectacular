(ns spectacular.lacinia
  (:require [clojure.string :as s]
            [clojure.set :refer [union]]
            ;;
            [camel-snake-kebab.core :as csk]
            [camel-snake-kebab.extras :as cske]
            ;;
            [spectacular.core :as sp]
            [spectacular.utils :refer [nsk?]]))

;;; --------------------------------------------------------------------------------
;;  Naming convention, policy, will make it configurable later.

(defn gql-type-key
  [k options]
  (or (::type options)               ; explicitly provided gql type
      (sp/-get k ::type)             ; gql type registered with entity
      ;;
      ;; Or if we are an attribute then our scalar may have a special
      ;; GQL type so check for that too, but otherwise use the
      ;; scalar type and NOT the attribute-key as the type
      (when (sp/attr? k)
        (let [scalar-key (sp/-get k ::sp/scalar-key)]
          (or (sp/-get scalar-key ::type)
              scalar-key)))
      ;;
      ;; Failing all of that we just use the original k
      k))

(defn gql-type-name
  [k {:keys [context kind]}]
  (let [type-part    (csk/->PascalCaseString k)
        kind-part    (case kind
                       :token   "Token"
                       :values  "Values"
                       ;; :record
                       "")
        context-part (case context
                       :input  "In"
                       ;; :output
                       "")]
    (-> (str type-part kind-part context-part)
        keyword)))

(defn gql-field-name
  [k {:keys [::is? ::has?]}]
  (let [k-name (name k)]
    (-> (if (s/ends-with? k-name "?")
          (str (cond
                 has? "has"
                 :else "is")
               (-> k-name
                   (s/replace #"\?" "")
                   csk/->PascalCaseString))
          (csk/->camelCaseString k))
        keyword)))

;;;

(defn listify
  [gql-type {:keys [many? required?]}]
  (cond->> gql-type
    (or many? required?) (list 'non-null)
    many?                (list 'list)))

(defn attr-type
  ([k] (-> k
           (gql-type-key  nil)
           (gql-type-name nil)))

  ([m k & [options]]
   (let [gql-type (-> k
                      (gql-type-key  options)
                      (gql-type-name options))]
     (assoc m :type (listify gql-type options)))))

(defn attr-description
  ([k]
   (or (sp/-get k ::description)
       (sp/-get k ::sp/description)))
  ;;
  ([m k & [{:keys [description] :as options}]]
   (if-let [val (or description
                    (attr-description k))]
     (assoc m :description val)
     m)))

;;; -- Enums

(defmulti enum->schema (fn [enum]
                           (cond
                             (sp/enum? enum) :enum-key
                             (map?     enum) :inline)))

(defmethod enum->schema :default
  [enum]
  (throw (ex-info "Don't know how to info to enum."
                  {:enum enum})))

(defmethod enum->schema :enum-key
  [k]
  (if-let [values (sp/values k)]
    ;; Enums are scalars so they have a type which maybe overridden on
    ;; the GQL side.  In the schema the type is in what we would
    ;; normally consider the 'key' position, so check for the GQL type
    ;; translation rather than a key translation.
    [(-> k
         (gql-type-key  nil)
         (gql-type-name nil))
     (-> {:values (mapv csk/->SCREAMING_SNAKE_CASE_KEYWORD values)}

         (attr-description k))]
    ;; Do I need this warning?  Doesn't spec ensure I can't do this?
    (throw (ex-info "Can't transform to an enum, key doesn't have any values."
                    {:enum-key k}))))

(defmethod enum->schema :inline
  [{:keys [key values description]}]
  [(csk/->PascalCaseKeyword key)
   (cond-> {:values (mapv csk/->SCREAMING_SNAKE_CASE_KEYWORD values)}
     description (assoc :description description))])

;;; --------------------------------------------------------------------------------

(defn canonicalise-attr
  [attr]
  (cond
    ;; :keyword
    (keyword? attr) {:type attr}

    ;; [:keyword]
    (and (vector? attr)
         (-> attr count (= 1))
         (-> attr first keyword?))
    {:type  (-> attr first)
     :many? true}

    ;; {:type :keyword}
    (and (map? attr)
         (-> attr :type keyword?))
    attr

    ;; {:type [:keyword]}
    (and (map? attr)
         (-> attr :type vector?)
         (-> attr :type count (= 1)))
    (assoc attr
           :type  (-> attr :type first)
           :many? true)

    :else (throw (ex-info "Invalid Attribute"
                          {:attr attr}))))

(defn attr->field
  [attr]
  (let [{k     :type
         :keys [resolver description]
         :as attr} (canonicalise-attr attr)]
    (cond-> (cond
              (sp/exists? k)
              (-> {}
                  (attr-type        k attr)
                  (attr-description k attr))
              ;;
              :else {:type (-> k csk/->PascalCaseKeyword (listify attr))})
      ;;
      resolver    (assoc :resolver    resolver)
      description (assoc :description description))))

;;; --------------------------------------------------------------------------------

(defmulti entity->output (fn [entity]
                           (cond
                             (sp/entity? entity)          :key
                             ;;
                             (map? entity) :inline)))

(defmethod entity->output :default
  [entity]
  (throw (ex-info "Don't know how to transform entity to output oject."
                  {:entity entity})))

(defmethod entity->output :key
  [k]
  (->> (sp/attribute-keys k)
       (map (fn [k]
              [(gql-field-name k nil) (attr->field k)]))
       (into {})))

(defmethod entity->output :inline
  [m]
  (->> m
       (map (fn [[k v]]
              [(gql-field-name k nil) (attr->field v)]))
       (into {})))

;;; --------------------------------------------------------------------------------

(defn get-entity-attributes
  [entity-key {:keys [token? values?]}]
  (let [required? (union (-> entity-key sp/identity-keys set)
                         (-> entity-key sp/required-keys set))]
    (->> (cond
           token?  (sp/identity-keys  entity-key)
           values? (sp/value-keys     entity-key)
           :else   (sp/attribute-keys entity-key))
         (mapv (fn [attr-key]
                 (-> (sp/get-attribute attr-key)
                     (assoc :type      attr-key
                            :required? (required? attr-key))))))))

(defmulti entity->input (fn [entity]
                          (cond
                            (sp/entity? entity)          :key
                            (-> entity :type sp/entity?) :map
                            ;;
                            (map? entity) :inline)))

(defmethod entity->input :default
  [entity]
  (throw (ex-info "Don't know how to transform entity to input oject."
                  {:entity entity})))

(defmethod entity->input :key
  [k]
  (->> (get-entity-attributes k nil)
       (map (fn [{k :type :as attr}]
              [(gql-field-name k nil)
               (attr->field    attr)]))
       (into {})))

(defmethod entity->input :map
  [{k     :type
    :as   m}]
  (->> (get-entity-attributes k m)
       (map (fn [{k :type :as attr}]
              [(gql-field-name k nil)
               (attr->field    attr)]))
       (into {})))

(defmethod entity->input :inline
  [m]
  (->> m
       (map (fn [[k v]]
              [(gql-field-name k nil) (attr->field v)]))
       (into {})))

;;; --------------------------------------------------------------------------------

(defn transform-query
  [query]
  {:type (->> query :type attr->field)
   :args (->> query
              :args
              (map (fn [[k v]]
                     [(gql-field-name k v)
                      (attr->field v)]))
              (into {}))})
