(ns spectacular.graphql
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]
            [clojure.set :refer [subset?]]
            ;;
            [camel-snake-kebab.core :as csk]
            [camel-snake-kebab.extras :as cske]
            ;;
            [spectacular.core :refer :all :as sp]))

;;;

(def +page-object+
  {:page {:object-type :graphql
          :fields      {:index {:gql-type :int :required? true :description "Zero based index of page."}
                        :size  {:gql-type :int :required? true :description "Size of page requested, records returned may be less than size."}}}})

(defn paged
  [entity-key]
  {:object-type :graphql
   :fields      {:total   {:gql-type :int       :required? true :description "Total number of matched results."}
                 :records {:gql-type entity-key :list? true :required? true}
                 :page    {:gql-type :page}}})

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

(defn field->gql-type
  [{::sp/keys [field-key scalar-key gql-type required? list?] :as record}
   & {:keys [optional? debug?] :as options}]
  (when debug? (println field-key record options))
  (let [required?       (if (contains? options :optional?)
                          (not optional?)
                          required?)
        gql-schema-type (-> (or gql-type scalar-key field-key)
                            name
                            csk/->PascalCaseSymbol)]
    (cond
      (and required? list?) `(~'non-null (~'list (~'non-null ~gql-schema-type)))
      required?             `(~'non-null ~gql-schema-type)
      list?                 `(~'list ~gql-schema-type)
      :else                 gql-schema-type)))

(defmulti transform-object (fn [object-key {:keys [object-type]}]
                             object-type))

(defmethod transform-object :entity-token
  [object-key {:keys [entity-key] :as record}]
  (if-let [fields (->> (get-identity-fields entity-key)
                       (map (fn [{::sp/keys [field-key description] :as field}]
                              [(csk/->camelCaseKeyword field-key)
                               (-> {:type (field->gql-type field)}
                                   (-assoc-description description))]))
                       (into {}))]
    [(csk/->PascalCaseKeyword object-key) {:fields fields}]
    (throw (ex-info "An Entity must have identity fields in order for it to be transformed it to an Entity Token."
                    {:object-key object-key :record record}))))

(defmethod transform-object :entity
  [object-key {:keys [entity-key] :as record}]
  [(csk/->PascalCaseKeyword object-key)
   (-> {:fields (->> (get-entity-fields entity-key)
                     (map (fn [{::sp/keys [field-key description] :as field}]
                            [(csk/->camelCaseKeyword field-key)
                             (-> {:type (field->gql-type field :optional? true)}
                                 (-assoc-description description))]))
                     (into {}))}
       (-assoc-description (get-entity-description entity-key)))])

(defmethod transform-object :graphql
  [object-key {:keys [fields description] :as record}]
  [(csk/->PascalCaseKeyword object-key)
   (-> {:fields (->> fields
                     (map (fn [[field-key {:keys [gql-type required? list? description] :as field}]]
                            [(csk/->camelCaseKeyword field-key)
                             ;; Raw graphql objects don't have the
                             ;; namespaced keywords, so make it
                             ;; conform to the spec'ed equivalents.
                             (-> {:type (field->gql-type (assoc field
                                                                ::sp/field-key field-key
                                                                ::sp/gql-type  gql-type
                                                                ::sp/required? required?
                                                                ::sp/list?     list?))}
                                 (-assoc-description description))]))
                     (into {}))}
       (-assoc-description description))])

;;; --------------------------------------------------------------------------------

(defmulti transform-input-object (fn [object-key {:keys [object-type]}]
                                   object-type))

(defmethod transform-input-object :entity-token
  [object-key {:keys [entity-key] :as record}]
  (if-let [fields (some->> entity-key
                           get-identity-fields
                           (map (fn [{::sp/keys [field-key] :as field}]
                                  [(csk/->camelCaseKeyword field-key)
                                   {:type (field->gql-type field)}]))
                           (into {})) ]
    [(csk/->PascalCaseKeyword object-key)
     (-> {:fields fields} (-assoc-description (get-entity-description entity-key)))]
    (throw (ex-info "An Entity must have Identity Fields in order for it to be transformed it to an Input Entity Token."
                    {:object-key object-key
                     :record     record}))))

(defmethod transform-input-object :entity
  [object-key {:keys [entity-key] :as record}]
  (if-let [fields  (some->> entity-key
                            get-entity-fields
                            (map (fn [{::sp/keys [field-key] :as field}]
                                   [(csk/->camelCaseKeyword field-key)
                                    {:type (field->gql-type field)}]))
                            (into {}))]
    [(csk/->PascalCaseKeyword object-key)
     (-> {:fields fields} (-assoc-description (get-entity-description entity-key)))]
    (throw (ex-info "An Entity must have Fields in order for it to be transformed it to an Input Entity."
                    {:object-key object-key
                     :record     record}))))

(defmethod transform-input-object :entity-content
  [object-key {:keys [entity-key] :as record}]
  (if-let [fields  (some->> entity-key
                            get-content-fields
                            (map (fn [{::sp/keys [field-key] :as field}]
                                   [(csk/->camelCaseKeyword field-key)
                                    {:type (field->gql-type field)}]))
                            (into {}))]
    [(csk/->PascalCaseKeyword object-key)
     (-> {:fields fields} (-assoc-description (get-entity-description entity-key)))]
    (throw (ex-info "An Entity must have Content Fields in order for it to be transformed it to an Input Entity."
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
  [{:keys [mutation-key mutation-type args description resolve] :as record}]
  (when-not mutation-key
    (throw (ex-info "Mutation must have a mutation-type" {:mutation record})))
  [(csk/->camelCaseKeyword mutation-key)
   (cond-> {:type (transform-return-type mutation-type)}
     args        (assoc :args        (->> args (map transform-arg) (into {})))
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

(defn make-schema
  [{:keys [enums objects input-objects queries mutations]}]
  (cond-> nil
    enums         (assoc :enums         (->> (mapv transform-enum          enums)         (into {})))
    objects       (assoc :objects       (->> (mapv -transform-object       objects)       (into {})))
    input-objects (assoc :input-objects (->> (mapv -transform-input-object input-objects) (into {})))
    queries       (assoc :queries       (->> (mapv -transform-query        queries)       (into {})))
    mutations     (assoc :mutations     (->> (mapv transform-mutation      mutations)     (into {})))))
