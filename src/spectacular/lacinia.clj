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

;;;

(defn attr-type
  ([k] (-> k
           (gql-type-key nil)
           (gql-type-name nil)))

  ([m k & [{:keys [required?] :as options}]]
   (let [gql (-> k
                 (gql-type-key options)
                 (gql-type-name options))]
     (assoc m :type (if required?
                      (list 'non-null gql)
                      gql)))))

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

;;;

(defn -type
  [k]
  (-> (or (sp/-get k ::type) k)
      csk/->PascalCaseKeyword))

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
  [ky]
  (or (sp/-get  ky ::label)
      (sp/label ky)))

(defn -description
  [ky]
  (or (sp/-get        ky ::description)
      (sp/description ky)))

(defn -description-map
  [ky]
  (when-let [description (-description ky)]
    {:description description}))

(defn -resolver
  [ky]
  (sp/-get ky ::resolver))

(defn -assoc-description
  [record ky & [description]]
  (if-let [description (or description (-description ky))]
    (assoc record :description description)
    record))

;;;

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

(defmulti attr->field (fn [attr]
                        (cond
                          (sp/scalar? attr) :sp-key
                          (sp/attr?   attr) :sp-key
                          (sp/entity? attr) :sp-key
                          ;;
                          (-> attr :type sp/scalar?) :sp-map
                          (-> attr :type sp/attr?)   :sp-map
                          (-> attr :type sp/entity?) :sp-map
                          ;;
                          (map? attr) :inline)))

(defmethod attr->field :default
  [attr]
  (throw (ex-info "Don't know how to transform attr to schema."
                  {:attr attr})))

(defmethod attr->field :sp-key
  [k]
  (-> {}
      (attr-type        k)
      (attr-description k)))

(defmethod attr->field :sp-map
  [{k     :type
    :keys [required? description] :as m}]
  (-> {}
      (attr-type        k m)
      (attr-description k m)))

(defmethod attr->field :inline
  [{:keys [required? description] :as m}]
  (let [gql-type (-> m :type csk/->PascalCaseKeyword)]
    (cond-> nil
      gql-type    (assoc :type (if required?
                                 (list 'non-null gql-type)
                                 gql-type))
      description (assoc :description description))))

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
  ;;
  ;; Attributes themselves don't know if they are required or not, so
  ;; the :required? option allows us to set that depending on the
  ;; context we are getting generated in.
  ;;
  ;; sp-inlines, inlines, can set it explicitly.
  ;;
  ;; Query objects enforce them to be optional, Input objects
  ;; determine it from the Entity.
  (some->> (for [[vk v] [[:type        (let [attr-type (-attr-type k)]
                                         (if required?
                                           (list 'non-null attr-type)
                                           attr-type))]
                         [:description (-description k)]
                         [:resolver    (-resolver    k)]]
                 :when v]
             [vk v])
           seq
           (into {})))

(defn -transform-inline
  [record {:keys [token? input?] :as options}]
  (let [required? (or (:required? record)
                      (:required? options))
        ;;
        attr-type (-> (cond-> (-> record :type name)
                        token? (str "-token")
                        input? (str "-input"))
                      csk/->PascalCaseSymbol)]
    (some->> (for [[k v] [[:type        (if required?
                                          (list 'non-null attr-type)
                                          attr-type)]
                          [:description (:description record)]
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

;;; --------------------------------------------------------------------------------

(defn -object-dispatcher
  [info]
  (let [sp?        #(and (nsk? %) (sp/entity? %))
        ;; It's an sp/entity but with overrides.
        sp-inline? #(and (map? %) (-> % :key sp/entity?))
        inline?    #(and (map? %)
                         (contains? % :key)
                         (contains? % :type))]
    (cond
      (sp?        info) :sp
      (sp-inline? info) :sp-inline
      (inline?    info) :inline)))

(defmulti transform-query-object -object-dispatcher)

(defmethod transform-query-object :sp
  [k]
  [(-type k)
   (merge {:fields (->> (sp/attribute-keys k)
                        (map transform-attr)
                        (into {}))}
          (-description-map k))])

;;;

(defmulti transform-input-object -object-dispatcher)

(defmethod transform-input-object :sp
  [k]
  (let [required-keys (-> k sp/required-keys set)]
    [(-type k)
     (merge {:fields (->> (sp/attribute-keys k)
                          (map (fn [k]
                                 (let [[gql-k {:keys [type] :as attrs}] (transform-attr k)]
                                   [gql-k (if (contains? required-keys k)
                                            (assoc attrs :type (list 'non-null type))
                                            attrs)])))
                          (into {}))}
            (-description-map k))]))

;;; --------------------------------------------------------------------------------

(def transform-query-arg nil)
(defmulti transform-query-arg (fn [key info]
                                (cond
                                  (and (-> info :type sp/entity?)
                                       (-> info :token?))
                                  :token
                                  ;;
                                  (-> info :type sp/entity?)
                                  :object
                                  ;;
                                  (map? info) :inline)))

(defmethod transform-query-arg :default
  [key info]
  (throw (ex-info "Don't know how to transform to query arg." {:key  key
                                                               :info info})))

(defmethod transform-query-arg :token
  [key info]
  {(->gql-key key info)
   (-transform-inline info  {:token? true})})

(defmethod transform-query-arg :object
  [key info]
  {(->gql-key key  info)
   (-transform-inline   info {:object? true})})

(defmethod transform-query-arg :inline
  [key info]
  {(->gql-key key  info)
   (-transform-inline   info nil)})

;;; --------------------------------------------------------------------------------

(defn transform-query-args
  [args]
  (->> args
       (map #(apply transform-query-arg %))
       (into {})))

;;; --------------------------------------------------------------------------------

(defn transform-query
  [query-name spec]

  )
