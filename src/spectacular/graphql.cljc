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
  [k]
  (or (sp/-get  k ::label)
      (sp/label k)))

(defn -description
  [k]
  (or (sp/-get        k ::description)
      (sp/description k)))

(defn -description-map
  [k]
  (when-let [description (-description k)]
    {:description description}))

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
  ;;
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

;;; --------------------------------------------------------------------------------

(defn -object-dispatcher
  [info & _]
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
