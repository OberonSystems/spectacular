(ns spectacular.graphql
   (:require [clojure.pprint :refer [pprint]]
             [clojure.spec.alpha :as s]
             [clojure.set :refer [subset?]]
             ;;
             [camel-snake-kebab.core :as csk]
             [camel-snake-kebab.extras :as cske]
             ;;
             [spectacular.core :as sp]
             [spectacular.utils :refer [nsk? #_map->nsmap]]))

(defn -type
  [k]
  (-> (or (sp/-get k ::type)
          k)
      csk/->PascalCaseSymbol))

(defn -attr-type
  [k]
  ;; First try to get the GQL type registered for the k.
  (-> (or (sp/-get k ::type)
          ;; Then as we are an attribute we get the scalar-key that is
          ;; registered for this attribute.
          (let [scalar-key (sp/-get k ::sp/scalar-key)]
            ;; Then we check again for GQL type registered for the
            ;; scalar
            (or (sp/-get k ::type)
                ;; Failing that we use the scalar-key as the type.
                scalar-key))
          k)
      csk/->PascalCaseSymbol))

(defn -label
  [k]
  (or (sp/-get  k ::label)
      (sp/label k)))

(defn -description
  [k]
  (or (sp/-get        k ::description)
      (sp/description k)))

(defn -resolver
  [k]
  (sp/-get k ::resolver))

(defn -assoc-description
  [record k]
  (let [description (-description k)]
    (cond-> record
      description (assoc :description description))))

;;; -- Enums

(defmulti transform-enum (fn [info]
                           (cond
                             (nsk? info) :sp
                             (map? info) :inline)))

(defmethod transform-enum :default
  [k]
  (throw (ex-info "Don't know how to transform to enum."
                  {:k k})))

(defmethod transform-enum :sp
  [k]
  (if-let [values (sp/values k)]
    [(-type k)
     (-> {:values (mapv csk/->SCREAMING_SNAKE_CASE_KEYWORD values)}
         (-assoc-description k))]
    (throw (ex-info "Can't transform to an enum, k doesn't have any values."
                    {:k k}))))

(defmethod transform-enum :inline
  [{:keys [key values description]}]
  [(csk/->PascalCaseKeyword key)
   (cond-> {:values (mapv csk/->SCREAMING_SNAKE_CASE_KEYWORD values)}
     description (assoc :description description))])

;;; -- Attributes

(defmulti transform-attr (fn [info & _]
                           (let [sp?        #(and (nsk? %) (sp/attr? %))
                                 sp-inline? #(and (map? %) (-> % :key sp/attr?))
                                 inline?    #(and (map? %)
                                                  (contains? % :key)
                                                  (contains? % :type))]
                             (cond
                               (sp?        info) :sp
                               (sp-inline? info) :sp-inline
                               (inline?    info) :inline
                               ;;
                               (and (vector? info)
                                    (= 1 (count info))
                                    (let [info (first info)]
                                      (or (sp?        info)
                                          (sp-inline? info)
                                          (inline?    info))))
                               :vector))))

(defmethod transform-attr :default
  [k & _]
  (throw (ex-info "Don't know how to transform to attribute."
                  k)))

(defn -transform-attr
  [k attr {:keys [required?] :as options}]
  ;; Attributes themselves don't know if they are required or not, so
  ;; the :required? option allows us to set that depending on the
  ;; context we are getting generated in.
  ;;
  ;; sp-inlines, inlines, can set it explicitly.
  ;;
  ;; Query objects enforce them to be optional, Input objects
  ;; determine it from the Entity.
  (some->> (for [[vk v] [[:type        (-attr-type   k)]
                         [:required?   (true?        required?)]
                         [:description (-description k)]
                         [:resolver    (-resolver    k)]]
                 :when v]
             [vk v])
           seq
           (into {})))

(defn -transform-inline
  [record {:as options}]
  (let [required? (or (:required? record)
                      (:required? options))]
    (some->> (for [[k v] [[:type        (some-> :type record csk/->PascalCaseSymbol)]
                          [:description (:description record)]
                          [:required?   (true?        required?)]
                          [:resolver    (:resolver    record)]]
                   :when v]
               [k v])
             seq
             (into {}))))

(defmethod transform-attr :sp
  [k & {:as options}]
  (let [attr (sp/get-attribute k)]
    [(csk/->PascalCaseKeyword k)
     (-transform-attr k attr options)]))

(defmethod transform-attr :sp-inline
  ;; It's a spec based attr but has some inline overrides.
  [{:keys [key] :as record} & {:as options}]
  (let [attr (sp/get-attribute key)]
    [(csk/->PascalCaseKeyword key)
     (merge (-transform-attr   key attr nil)
            (-transform-inline record options))]))

(defmethod transform-attr :inline
  [record & {:as options}]
  [(csk/->PascalCaseKeyword key)
   (merge (-transform-inline record options))])

(defmethod transform-attr :vector
  [v & {:as options}]
  (let [{:keys [required?]} (meta v)
        ;;
        ;; FIXME, Could also have a scalar, or an entity in the attribute
        ;; position.
        ;;
        [_ attr] (transform-attr (first v))
        ;; Resolvers don't make any sense within a list context
        attr (dissoc attr :resolver)]
    (if required?
      (list 'list (list 'non-null attr))
      (list 'list attr))))

(comment

;;;

  (defn m->sp
    [m]
    (map->nsmap m 'spectacular.core))

  (defn gql-field->field
    [{:keys [field-key type] :as record}]
    (let [type (or type field-key)]
      (cond
        (entity? type) (merge (get-entity           type) record)
        (field?  type) (merge (get-field-and-scalar type) record)
        (scalar? type) (merge (get-scalar           type) record)
        :else record)))

  (defn gql-fields->fields
    [fields]
    ;; Graphql fields come in as a map and we need to assoc the key of
    ;; the map into the values to make it conform to the shape of a
    ;; spectacular/field.
    (->> fields
         (map (fn [[k v]]
                (-> v
                    (assoc :field-key k)
                    gql-field->field)))))

;;;

  (def +page-object+
    {:page {:object-type :graphql
            :fields      {:index {:type :int :required? true :description "Zero based index of page."}
                          :size  {:type :int :required? true :description "Max number of records to include in each page."}}}})

  (defn paged
    [entity-key]
    {:object-type :graphql
     :fields      {:total   {:type :int       :required? true :description "Total number of matched results."}
                   :records {:type entity-key :list? true :required? true :description nil}
                   :page    {:type :page}}})

;;; --------------------------------------------------------------------------------

  (defn rename-to-object-in
    [type-key]
    (when (sp/entity? type-key)
      (-> (name type-key)
          (str "-in")
          symbol)))

  (defn transform-field
    [{sp-field-key   ::sp/field-key
      sp-gql-type    ::sp/gql-type
      sp-scalar-key  ::sp/scalar-key
      sp-label       ::sp/label
      sp-description ::sp/description
      sp-required?   ::sp/required?
      :keys [field-key type label description args resolve required? list?]
      :as record}
     & {:keys [rename-type optional? debug?] :as options}]
    (when debug? (println record))
    (when-not (or field-key sp-field-key)
      (throw (ex-info "Must have a Field key" record)))
    ;;
    ;; Respect nil overrides, ie, :description nil should suppress the
    ;; sp-description.
    (let [label (if (contains? record :label)
                  label
                  sp-label)
          desc  (if (contains? record :description)
                  description
                  sp-description)
          ;;
          required?       (if (contains? options :optional?)
                            (not optional?)
                            (or required? sp-required?))
          gql-schema-type (-> (or type sp-gql-type
                                  sp-scalar-key
                                  field-key sp-field-key)
                              (as-> type-key
                                  (if rename-type
                                    (or (rename-type type-key) type-key)
                                    type-key))
                              name
                              csk/->PascalCaseSymbol)]
      [(-> (or field-key sp-field-key) csk/->camelCaseKeyword)
       (cond-> {:type (cond
                        (and required? list?) `(~'non-null (~'list (~'non-null ~gql-schema-type)))
                        required?             `(~'non-null ~gql-schema-type)
                        list?                 `(~'list (~'non-null ~gql-schema-type))
                        :else                 gql-schema-type)}

         desc    (assoc :description desc)
         args    (assoc :args        (->> args
                                          gql-fields->fields
                                          (map transform-field)
                                          (into {})))
         resolve (assoc :resolve     resolve))]))

  (defn transform-return-type
    [k list?]
    (let [k (or (-> (or (and (entity? k) (get-entity k))
                        (and (field?  k) (get-field  k))
                        (and (scalar? k) (get-scalar k)))
                    ::sp/gql-type)
                k)
          gql-schema-type (csk/->PascalCaseSymbol k)]
      (if list?
        `(~'list (~'non-null ~gql-schema-type))
        gql-schema-type)))

;;; --------------------------------------------------------------------------------

  (defmulti transform-object (fn [object-key {:keys [object-type]}]
                               object-type))

  (defmethod transform-object :entity-token
    [object-key {:keys [entity-key exclude fields] :as record}]
    (if-let [fields (->> (concat (get-identity-fields entity-key :exclude exclude)
                                 (gql-fields->fields  fields))
                         (map transform-field)
                         (into {}))]
      [(csk/->PascalCaseKeyword object-key) {:fields fields}]
      (throw (ex-info "An Entity must have identity fields in order for it to be transformed it to an Entity Token."
                      {:object-key object-key :record record}))))

  (defmethod transform-object :entity
    [object-key {:keys [entity-key exclude fields] :as record}]
    [(csk/->PascalCaseKeyword object-key)
     (-> {:fields (->> (concat (get-entity-fields  entity-key nil :exclude exclude)
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
    [object-key {:keys [entity-key fields rename-type] :as record}]
    (if-let [fields (some->> (concat (get-identity-fields entity-key)
                                     (gql-fields->fields fields))
                             (map #(transform-field % :rename-type rename-type))
                             (into {}))]
      [(csk/->PascalCaseKeyword object-key)
       (-> {:fields fields} (-assoc-description (get-entity-description entity-key)))]
      (throw (ex-info "An Entity must have Identity Fields in order for it to be transformed it to an Token Input Object."
                      {:object-key object-key
                       :record     record}))))

  (defmethod transform-input-object :entity
    [object-key {:keys [entity-key exclude fields rename-type] :as record}]
    (if-let [fields  (some->> (concat (get-entity-fields entity-key nil :exclude exclude)
                                      (gql-fields->fields fields))
                              (map #(transform-field % :rename-type rename-type))
                              (into {}))]
      [(csk/->PascalCaseKeyword object-key)
       (-> {:fields fields} (-assoc-description (get-entity-description entity-key)))]
      (throw (ex-info "An Entity must have Fields in order for it to be transformed it to an Entity Input Object."
                      {:object-key object-key
                       :record     record}))))

  (defmethod transform-input-object :entity-content
    [object-key {:keys [entity-key exclude fields rename-type] :as record}]
    (if-let [fields  (some->> (concat (get-content-fields entity-key :exclude exclude)
                                      (gql-fields->fields fields))
                              (map #(transform-field % :rename-type rename-type))
                              (into {}))]
      [(csk/->PascalCaseKeyword object-key)
       (-> {:fields fields} (-assoc-description (get-entity-description entity-key)))]
      (throw (ex-info "An Entity must have Content Fields in order for it to be transformed it to a Content Input Object."
                      {:object-key object-key
                       :record     record}))))

;;;

  (defn transform-query
    [query-key {:keys [args type list? description resolve] :as record}]
    (when-not type
      (throw (ex-info "Query must have a return type" {:query-key query-key
                                                       :record    record})))
    [(csk/->camelCaseKeyword query-key)
     (cond-> {:type (transform-return-type type list?)}
       args        (assoc :args        (->> args
                                            gql-fields->fields
                                            (map transform-field)
                                            (into {})))
       resolve     (assoc :resolve     resolve)
       description (assoc :description description))])

;;;

  (defn transform-mutation
    [mutation-key {:keys [args type list? description resolve] :as record}]
    (when-not type
      (throw (ex-info "Mutation must have a type" {:mutation-key mutation-key
                                                   :record       record})))
    [(csk/->camelCaseKeyword mutation-key)
     (cond-> {:type (transform-return-type type list?)}
       args        (assoc :args        (->> args
                                            gql-fields->fields
                                            (map transform-field)
                                            (into {})))
       resolve     (assoc :resolve     resolve)
       description (assoc :description description))])

;;;

  (defn -map-transform
    [f coll]
    (map #(f (first %) (second %)) coll))

  (defn make-schema
    [{:keys [enums objects input-objects queries mutations]}]
    (let []
      (cond-> nil
        enums         (assoc :enums         (->> enums         (map transform-enum)                    (into {})))
        objects       (assoc :objects       (->> objects       (-map-transform transform-object)       (into {})))
        input-objects (assoc :input-objects (->> input-objects (-map-transform transform-input-object) (into {})))
        queries       (assoc :queries       (->> queries       (-map-transform transform-query)        (into {})))
        mutations     (assoc :mutations     (->> mutations     (-map-transform transform-mutation)     (into {}))))))
  )
