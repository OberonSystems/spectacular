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
  [{::sp/keys [field-key scalar-key gql-type optional?]}
   & {:keys [force-optional? debug?]}]
  (when debug?
    (println field-key scalar-key gql-type optional?))
  (let [gql-schema-type (-> (or gql-type (-> scalar-key name))
                            csk/->PascalCaseSymbol)]
    (cond
      force-optional? gql-schema-type
      optional?       gql-schema-type
      :else           `(~'non-null ~gql-schema-type))))

(defmulti transform-object :object-type)

(defmethod transform-object :entity-token
  [{:keys [entity-key] :as record}]
  (if-let [fields (some->> (get-identity-fields entity-key)
                           (map (fn [{::sp/keys [field-key description] :as field}]
                                  [(csk/->camelCaseKeyword field-key)
                                   (-> {:type (field->gql-type field)}
                                       (-assoc-description description))]))
                           (into {}))]
    [(-> (csk/->PascalCaseString entity-key) (str "Token") keyword)
     {:fields fields}]
    (throw (ex-info "An Entity must have Identity Keys in order for it to be transformed it to an Entity Token."
                    {:entity-key entity-key}))))

(defmethod transform-object :entity
  [{:keys [entity-key] :as record}]
  [(csk/->PascalCaseKeyword entity-key)
   (-> {:fields (->> (get-entity-fields entity-key)
                     (map (fn [{::sp/keys [field-key description] :as field}]
                            [(csk/->camelCaseKeyword field-key)
                             (-> {:type (field->gql-type field :force-optional? true)}
                                 (-assoc-description description))]))
                     (into {}))}
       (-assoc-description (get-entity-description entity-key)))])

(defmethod transform-object :graphql
  [{:keys [graphql-key fields description] :as record}]
  [(csk/->PascalCaseKeyword graphql-key)
   (-> {:fields (->> fields
                     (map (fn [[key graphql-type description]]
                            [(csk/->camelCaseKeyword key)
                             (-> {:type graphql-type}
                                 (-assoc-description description))]))
                     (into {}))}
       (-assoc-description description))])

;;;

(defmulti transform-input-object :object-type)

(defmethod transform-input-object :entity-token
  [{:keys [entity-key]}]
  (if-let [fields (some->> entity-key
                           get-identity-fields
                           (map (fn [{::sp/keys [field-key] :as field}]
                                  [(csk/->camelCaseKeyword field-key)
                                   {:type (field->gql-type field)}]))
                           (into {})) ]
    [(-> (csk/->PascalCaseString entity-key) (str "InToken") keyword)
     (-> {:fields fields} (-assoc-description (get-entity-description entity-key)))]
    (throw (ex-info "An Entity must have Identity Keys in order for it to be transformed it to an Input Entity Token."
                    {:entity-key entity-key}))))

(defmethod transform-input-object :entity-content
  [{:keys [generate entity-key]}]
  (if-let [fields  (some->> entity-key
                            get-content-fields
                            (map (fn [{::sp/keys [field-key] :as field}]
                                   [(csk/->camelCaseKeyword field-key)
                                    {:type (field->gql-type field)}]))
                            (into {}))]
    [(-> (csk/->PascalCaseString entity-key) (str "In") keyword)
     (-> {:fields fields} (-assoc-description (get-entity-description entity-key)))]
    (throw (ex-info "An Entity must have Content Keys in order for it to be transformed it to an Input Entity."
                    {:entity-key entity-key}))))
