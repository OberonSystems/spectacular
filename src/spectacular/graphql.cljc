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

(defn paged-records
  [record-type]
  {(-> (str (name record-type) "-records") keyword)
   {:object-type :graphql
    :fields      {:total   {:gql-type :int        :required? true :description "Total number of matched results."}
                  :records {:gql-type record-type :list? true}
                  :page    {:gql-type :records-page}}}})

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
  [{::sp/keys [field-key scalar-key gql-type optional? list?]}
   & {:keys [force-optional? debug?]}]
  (when debug?
    (println field-key scalar-key gql-type optional?))
  (let [gql-schema-type (-> (or gql-type scalar-key field-key)
                            name
                            csk/->PascalCaseSymbol)]
    (cond
      list?           `(~'list ~gql-schema-type)
      force-optional? gql-schema-type
      optional?       gql-schema-type
      :else           `(~'non-null ~gql-schema-type))))

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
                             (-> {:type (field->gql-type field :force-optional? true)}
                                 (-assoc-description description))]))
                     (into {}))}
       (-assoc-description (get-entity-description entity-key)))])

(defmethod transform-object :graphql
  [object-key {:keys [fields description] :as record}]
  [(csk/->PascalCaseKeyword object-key)
   (-> {:fields (->> fields
                     (map (fn [field-key field]
                            [(csk/->camelCaseKeyword field-key)
                             (-> {:type (field->gql-type field)}
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

(defmulti transform-arg (fn [arg-key {:keys [arg-type] :as record}]
                          (cond
                            (scalar? arg-type) :scalar
                            (field?  arg-type) :field
                            (entity? arg-type) :entity
                            ;;
                            (keyword? arg-type) :graphql
                            :else (throw (ex-info "Unknown arg type" {:arg-key arg-key :record record})))))

(defmethod transform-arg :scalar
  [arg-key {:keys [arg-type required? description]}]
  (let [{::sp/keys [graphql-type] :as record} (get-scalar arg-type)]
    [(csk/->camelCaseKeyword arg-key)
     (-> {:type (maybe-non-null (or graphql-type arg-key) required?)}
         (-assoc-description (or description (::sp/description record))))]))

(defmethod transform-arg :field
  [arg-key {:keys [arg-type required? description]}]
  (let [{::sp/keys [gql-type scalar-key] :as record} (get-field arg-type)]
    [(csk/->camelCaseKeyword arg-key)
     (-> {:type (maybe-non-null (or gql-type scalar-key) required?)}
         (-assoc-description (or description (::sp/description record))))]))

(defmethod transform-arg :entity
  [arg-key {:keys [arg-type required? description]}]
  (let [record (get-entity arg-type)]
    [(csk/->camelCaseKeyword arg-key)
     (-> {:type (maybe-non-null arg-type required?)}
         (-assoc-description (or description (::sp/description record))))]))

(defmethod transform-arg :graphql
  [arg-key {:keys [arg-type description required?] :as record}]
  [(csk/->camelCaseKeyword arg-key)
   (-> {:type (maybe-non-null arg-type required?)}
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
  [query-key {:keys [args return-type description resolve] :as record}]
  (when-not return-type
    (throw (ex-info "Query must have a return-type" {:query-key query-key
                                                     :record    record})))
  [(csk/->camelCaseKeyword query-key)
   (cond-> {:type (transform-return-type return-type)}
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
