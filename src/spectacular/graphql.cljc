(ns spectacular.graphql
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clojure.set :refer [subset?]]
            ;;
            [camel-snake-kebab.core :as csk]
            [camel-snake-kebab.extras :as cske]
            ;;
            [spectacular.core :refer :all :as sp]
            [spectacular.utils :refer [map->nsmap]]))

;;;

(defn m->sp
  [m]
  (map->nsmap m 'spectacular.core))

;;;

(def +page-object+
  {:page {:object-type :graphql
          :fields      {:index {:type :int :required? true :description "Zero based index of page."}
                        :size  {:type :int :required? true :description "Max number of records to include in each page."}}}})

(defn paged
  [entity-key]
  {:object-type :graphql
   :fields      {:total   {:type :int       :required? true :description "Total number of matched results."}
                 :records {:type entity-key :list? true :required? true}
                 :page    {:type :page}}})

;;;

(defn -assoc-description
  [record description]
  (cond-> record
    description (assoc :description description)))

;;; --------------------------------------------------------------------------------

(defmulti transform-enum (fn [info]
                           (cond
                             (keyword? info) :spec
                             (vector?  info) :inline)))

(defmethod transform-enum :spec
  [spec]
  [(csk/->PascalCaseKeyword spec)
   (-> {:values (mapv csk/->SCREAMING_SNAKE_CASE_KEYWORD (get-enum-values spec))}
       (-assoc-description (get-scalar spec :description)))])

(defmethod transform-enum :inline
  [[key values description]]

  [(csk/->PascalCaseKeyword key)
   (-> {:values (mapv csk/->SCREAMING_SNAKE_CASE_KEYWORD values)}
       (-assoc-description description))])

;;; --------------------------------------------------------------------------------

(defn gql-fields->fields
  [fields]
  ;; Graphql fields come in as a map and we need to assoc the key of
  ;; the map into the values to make it conform to the shape of a
  ;; spectacular/field.
  (->> fields
       (map (fn [[k v]]
              (assoc v :field-key k)))))

(defn transform-field
  [{sp-field-key   ::sp/field-key
    sp-gql-type    ::sp/gql-type
    sp-scalar-key  ::sp/scalar-key
    sp-label       ::sp/label
    sp-description ::sp/description
    sp-required?   ::sp/required?
    :keys [field-key type label description resolve required? list?]
    :as record}
   & {:keys [optional? debug?] :as options}]
  (when debug? (println record))
  (when-not (or field-key sp-field-key)
    (throw (ex-info "Must have a Field key" record)))
  ;;
  (let [label (or label       sp-label)
        desc  (or description sp-description)
        ;;
        required?       (if (contains? options :optional?)
                          (not optional?)
                          (or required? sp-required?))
        gql-schema-type (-> (or type sp-gql-type
                                sp-scalar-key
                                field-key sp-field-key)
                            name
                            csk/->PascalCaseSymbol)]
    [(csk/->camelCaseKeyword (or field-key sp-field-key))
     (cond-> {:type (cond
                      (and required? list?) `(~'non-null (~'list (~'non-null ~gql-schema-type)))
                      required?             `(~'non-null ~gql-schema-type)
                      list?                 `(~'list (~'non-null ~gql-schema-type))
                      :else                 gql-schema-type)}

       desc    (assoc :description desc)
       resolve (assoc :resolve     resolve))]))

;;; --------------------------------------------------------------------------------

(defmulti transform-object (fn [object-key {:keys [object-type]}]
                             object-type))

(defmethod transform-object :entity-token
  [object-key {:keys [entity-key] :as record}]
  (if-let [fields (->> (get-identity-fields entity-key)
                       (map transform-field)
                       (into {}))]
    [(csk/->PascalCaseKeyword object-key) {:fields fields}]
    (throw (ex-info "An Entity must have identity fields in order for it to be transformed it to an Entity Token."
                    {:object-key object-key :record record}))))

(defmethod transform-object :entity
  [object-key {:keys [entity-key fields] :as record}]
  [(csk/->PascalCaseKeyword object-key)
   (-> {:fields (->> (concat (get-entity-fields  entity-key)
                             (gql-fields->fields fields))
                     (map #(transform-field % :optional? true))
                     (into {}))}
       (-assoc-description (get-entity-description entity-key)))])

(defmethod transform-object :graphql
  [object-key {:keys [fields description] :as record}]
  [(csk/->PascalCaseKeyword object-key)
   (-> {:fields (->> fields
                     gql-fields->fields
                     (map transform-field)
                     (into {}))}
       (-assoc-description description))])

;;; --------------------------------------------------------------------------------

(defmulti transform-input-object (fn [object-key {:keys [object-type]}]
                                   object-type))

(defmethod transform-input-object :entity-token
  [object-key {:keys [entity-key] :as record}]
  (if-let [fields (some->> entity-key
                           get-identity-fields
                           (map transform-field)
                           (into {})) ]
    [(csk/->PascalCaseKeyword object-key)
     (-> {:fields fields} (-assoc-description (get-entity-description entity-key)))]
    (throw (ex-info "An Entity must have Identity Fields in order for it to be transformed it to an Token Input Object."
                    {:object-key object-key
                     :record     record}))))

(defmethod transform-input-object :entity
  [object-key {:keys [entity-key] :as record}]
  (if-let [fields  (some->> entity-key
                            get-entity-fields
                            (map transform-field)
                            (into {}))]
    [(csk/->PascalCaseKeyword object-key)
     (-> {:fields fields} (-assoc-description (get-entity-description entity-key)))]
    (throw (ex-info "An Entity must have Fields in order for it to be transformed it to an Entity Input Object."
                    {:object-key object-key
                     :record     record}))))

(defmethod transform-input-object :entity-content
  [object-key {:keys [entity-key] :as record}]
  (if-let [fields  (some->> entity-key
                            get-content-fields
                            (map transform-field)
                            (into {}))]
    [(csk/->PascalCaseKeyword object-key)
     (-> {:fields fields} (-assoc-description (get-entity-description entity-key)))]
    (throw (ex-info "An Entity must have Content Fields in order for it to be transformed it to a Content Input Object."
                    {:object-key object-key
                     :record     record}))))

;;;

(defn maybe-non-null
  [sym non-null?]
  (let [sym (csk/->PascalCaseSymbol sym)]
    (if non-null?
      `(~'non-null ~sym)
      sym)))

(defmulti transform-arg (fn [arg-key {:keys [type] :as record}]
                          (cond
                            (scalar? type) :scalar
                            (field?  type) :field
                            (entity? type) :entity
                            ;;
                            (keyword? type) :graphql
                            :else (throw (ex-info "Unknown arg type" {:arg-key arg-key
                                                                      :type    type})))))

(defmethod transform-arg :scalar
  [arg-key {:keys [type required? description]}]
  (let [{::sp/keys [gql-type] :as record} (get-scalar type)]
    [(csk/->camelCaseKeyword arg-key)
     (-> {:type (maybe-non-null (or gql-type arg-key) required?)}
         (-assoc-description (or description (::sp/description record))))]))

(defmethod transform-arg :field
  [arg-key {:keys [type required? description]}]
  (let [{::sp/keys [gql-type scalar-key] :as record} (get-field type)]
    [(csk/->camelCaseKeyword arg-key)
     (-> {:type (maybe-non-null (or gql-type scalar-key) required?)}
         (-assoc-description (or description (::sp/description record))))]))

(defmethod transform-arg :entity
  [arg-key {:keys [type required? description]}]
  (let [record (get-entity type)]
    [(csk/->camelCaseKeyword arg-key)
     (-> {:type (maybe-non-null type required?)}
         (-assoc-description (or description (::sp/description record))))]))

(defmethod transform-arg :graphql
  [arg-key {:keys [type description required?] :as record}]
  [(csk/->camelCaseKeyword arg-key)
   (-> {:type (maybe-non-null type required?)}
       (-assoc-description description))])

(defn -transform-arg
  [[arg1 arg2]]
  (transform-arg arg1 arg2))

(defn transform-return-type
  [k]
  (let [k (or (-> (or (and (entity? k) (get-entity k))
                      (and (field?  k) (get-field  k))
                      (and (scalar? k) (get-scalar k)))
                  ::sp/gql-type)
              k)]
    (csk/->PascalCaseSymbol k)))

(defn transform-query
  [query-key {:keys [args type description resolve] :as record}]
  (when-not type
    (throw (ex-info "Query must have a return type" {:query-key query-key
                                                     :record    record})))
  [(csk/->camelCaseKeyword query-key)
   (cond-> {:type (transform-return-type type)}
     args        (assoc :args        (->> args (map -transform-arg) (into {})))
     resolve     (assoc :resolve     resolve)
     description (assoc :description description))])

;;;

(defn transform-mutation
  [mutation-key {:keys [args type description resolve] :as record}]
  (when-not type
    (throw (ex-info "Mutation must have a type" {:mutation-key mutation-key
                                                 :record       record})))
  [(csk/->camelCaseKeyword mutation-key)
   (cond-> {:type (transform-return-type type)}
     args        (assoc :args        (->> args (map -transform-arg) (into {})))
     resolve     (assoc :resolve     resolve)
     description (assoc :description description))])

;;;

(defn -transform-object
  [[arg1 arg2]]
  (transform-object arg1 arg2))

(defn -transform-input-object
  [[arg1 arg2]]
  (transform-input-object arg1 arg2))

(defn -transform-query
  [[arg1 arg2]]
  (transform-query arg1 arg2))

(defn -transform-mutation
  [[arg1 arg2]]
  (transform-mutation arg1 arg2))

(defn make-schema
  [{:keys [enums objects input-objects queries mutations]}]
  (cond-> nil
    enums         (assoc :enums         (->> (mapv transform-enum          enums)         (into {})))
    objects       (assoc :objects       (->> (mapv -transform-object       objects)       (into {})))
    input-objects (assoc :input-objects (->> (mapv -transform-input-object input-objects) (into {})))
    queries       (assoc :queries       (->> (mapv -transform-query        queries)       (into {})))
    mutations     (assoc :mutations     (->> (mapv -transform-mutation     mutations)     (into {})))))
