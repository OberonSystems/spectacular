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
  [{::sp/keys [field-key scalar-key gql-type]}
   & {:keys [optional?]}]
  (let [gql-schema-type (-> (or gql-type (-> scalar-key name))
                            csk/->PascalCaseSymbol)]
    (if optional?
      gql-schema-type
      `(~'non-null ~gql-schema-type))))

(defmulti transform-object :object-type)

(defmethod transform-object :entity-token
  [{:keys [entity-key] :as record}]
  (let [ks (get-entity-identity entity-key)
        _  (when-not ks (throw (ex-info "Entity must have 'identity' to transform it ot an Entity Token"
                                        {:entity-key entity-key})))]
    [(-> (csk/->PascalCaseString entity-key) (str "Token") keyword)
     {:fields (->> (get-fields ks)
                   (map (fn [{::sp/keys [field-key] :as field}]
                          [(csk/->camelCaseKeyword field-key)
                           {:type (field->gql-type field :optional? false)}]))
                   (into {}))}]))

(defmethod transform-object :entity
  [{:keys [entity-key] :as record}]
  [(csk/->PascalCaseKeyword entity-key)
   (-> {:fields (->> (get-entity-fields entity-key)
                     (map (fn [{::sp/keys [field-key gql-type] :as field}]
                            [(csk/->camelCaseKeyword field-key)
                             {:type (field->gql-type field)}]))
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

(defmethod transform-input-object :entity
  [{:keys [generate entity-key]}]
  (let [entity-name (csk/->PascalCaseString entity-key)
        identity-ks (get-entity-identity entity-key)
        content-ks  (get-entity-content  entity-key)]
    [(when identity-ks
       [(-> (str entity-name "InToken") keyword)
        (-> {:fields (->> identity-ks
                          get-fields
                          (map (fn [{::sp/keys [field-key] :as field}]
                                 [(csk/->camelCaseKeyword field-key)
                                  {:type (field->gql-type field :optional? false)}]))
                          (into {}))}
            (-assoc-description (get-entity-description entity-key)))])
     (when content-ks
       [(-> (str entity-name "In") keyword)
        (-> {:fields (->> content-ks
                          get-fields
                          (map (fn [{::sp/keys [field-key optional?] :as field}]
                                 [(csk/->camelCaseKeyword field-key)
                                  {:type (field->gql-type field :optional? optional?)}]))
                          (into {}))}
            (-assoc-description (get-entity-description entity-key)))])]))
