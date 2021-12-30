(ns spectacular.lacinia
   (:require [clojure.string :as s]
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

(defn attr-type
  ([k] (-> k
           (gql-type-key nil)
           (gql-type-name nil)))

  ([m k & [{:keys [many? required?] :as options}]]
   (let [gql (-> k
                 (gql-type-key  options)
                 (gql-type-name options))]
     (assoc m :type (cond->> gql
                      required? (list 'non-null)
                      many?     (list 'list))))))

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

(defn -attr->field
  [attr]
  (let [[arity attr] (if (and (vector? attr)
                              (-> attr count (= 1)))
                       [:many (first attr)]
                       [:one  attr])
        ;;
        kind (cond
               (sp/scalar? attr) :key
               (sp/attr?   attr) :key
               (sp/entity? attr) :key
               ;;
               (-> attr :type sp/scalar?) :map
               (-> attr :type sp/attr?)   :map
               (-> attr :type sp/entity?) :map
               ;;
               (map? attr) :inline)]
    (when kind [arity kind])))

(def attr->field nil)
(defmulti attr->field -attr->field)

(defmethod attr->field :default
  [attr]
  (throw (ex-info "Don't know how to transform attr to schema."
                  {:attr attr})))

(defmethod attr->field [:one :key]
  [k]
  (-> {}
      (attr-type        k)
      (attr-description k)))

(defmethod attr->field [:many :key]
  [[k]]
  (attr->field {:type      k
                :required? true
                :many?     true}))

(defmethod attr->field [:one :map]
  [{k     :type
    :keys [required? description] :as m}]
  (-> {}
      (attr-type        k m)
      (attr-description k m)))

(defmethod attr->field [:many :map]
  [[attr]]
  (-> attr
      (assoc :required? true
             :many?     true)
      attr->field))

(defmethod attr->field [:one :inline]
  [{:keys [required? description] :as m}]
  (let [gql-type (-> m :type csk/->PascalCaseKeyword)]
    (cond-> nil
      gql-type    (assoc :type (if required?
                                 (list 'non-null gql-type)
                                 gql-type))
      description (assoc :description description))))

(defmethod attr->field [:many :inline]
  [[m]]
  (-> m
      (assoc :required? true
             :many?     true)
      attr->field))

;;; --------------------------------------------------------------------------------

(defmulti entity->output-object (fn [entity]
                                  (cond
                                    (sp/entity? entity)          :key
                                    (-> entity :type sp/entity?) :map
                                    ;;
                                    (map? entity) :inline)))

(defmethod entity->output-object :default
  [entity]
  (throw (ex-info "Don't know how to transform entity to output oject."
                  {:entity entity})))

(defmethod entity->output-object :key
  [k]
  (->> (sp/attribute-keys k)
       (map (fn [k]
              [(gql-field-name k nil) (attr->field k)]))
       (into {})))

(defmethod entity->output-object :map
  [info])

(defmethod entity->output-object :inline
  [m])

;;; --------------------------------------------------------------------------------

(defn transform-query
  [{:keys [type args]}]
  {:type {:blah :blah}
   :args (->> args
              (map (fn [[k v]]
                     [(gql-field-name k v)
                      (attr->field v)]))
              (into {}))})
