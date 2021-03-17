(ns spectacular.core-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            ;;
            [camel-snake-kebab.core :as csk]
            [camel-snake-kebab.extras :as cske])
  (:require [spectacular.core :refer :all]
            :reload))

;;; Some globals to then test with

(register-enum ::guitar-brand
               [::fender ::gibson ::prs ::martin ::gretch]
               :captions {::prs "Paul Read Smith"}
               :description "A small selection of Guitar Brands")

;;;

(register-data-type ::string      string?
                    :graphql-type ::string
                    :db-type      ::text)

(register-data-type ::integer     int?
                    :graphql-type ::string
                    :db-type      ::text)

(register-data-type ::uuid        uuid?
                    :graphql-type ::uuid
                    :db-type      ::uuid)

;;;

(register-field ::guitar-uuid ::uuid
                :caption      "Guitar ID"
                :description  "Globally Unique ID.")

(register-field ::name        ::string
                :caption      "Guitar"
                :description  "Usually a six stringed instrument.")

(register-field ::guitar-brand ::guitar-brand
                :caption      "Guitar Brand")

(register-field ::age         ::integer
                :caption      "Age"
                :description  "Age in years.")

(register-entity ::guitar   [::guitar-uuid ::guitar-brand ::name ::age]
                 :identity  [::guitar-uuid]
                 :optional? #{::age}
                 :fields    {::name {:caption     "Guitar Name"
                                     :description "BB King called his 'Lucille'."}})

(deftest testing-something
  (testing "something"
    (is (s/valid? ::integer 10))
    (is (not (s/valid? ::integer "100")))
    ;;
    (is (s/valid? ::guitar {::guitar-uuid  #uuid "2f67ecd2-196b-4cc8-8b43-4b0d11d614f9"
                            ::guitar-name  "Lucille"
                            ::guitar-brand ::gibson
                            ::age          10}))))

(def +test-schema+
  {:enums   [::guitar-brand
             ;;
             ;; Or declare inline
             [:just-for-graphql-1 [:graph1 :graph2 :graph3] "A Description"]
             [:just-for-graphql-2 [:graph1 :graph2 :graph3]]]
   ;;
   :objects [{:entity ::brand :exclude [::field1 ::field2]}
             ;;
             {:entity ::guitar :token? true}
             {:entity ::guitar :include [{::brand {:resolve 'lookup-brand}}]}
             ;;
             ;; Can also declare inline if need be.
             {:object :pick
              :fields [[:colour 'String "Colour"]
                       [:brand  '(no-null String) "Brand of Pick"]]}]
   :queries {:fetch-guitars {:type    [::guitar]
                             :args    [::brand]
                             :resolve 'fetch-guitars}
             ;;
             :fetch-guitar  {:type    ::guitar
                             :args    [::guitar-uuid
                                       {:key         :brand-like
                                        :type        :string
                                        :description "Wild card search for guitar."}]
                             :resolve 'fetch-guitar}}
   ;;
   :input-objects [{:entity ::guitar :generate :all}]
   :mutations     {:add-guitar    {:type ::guitar
                                   :args [:guitar-input-add]
                                   :resolve '(add-entity ::guitar %)}
                   :modify-guitar {:type ::guitar
                                   :args [:guitar-input-modify]
                                   :resolve '(modify-entity ::guitar %)}
                   :remove-guitar {:type 'boolean
                                   :args [:guitar-input-token]}}})

(defn -assoc-description
  [record description]
  (cond-> record
    description (assoc :description description)))

(defmulti transform-enum (fn [info]
                           (cond
                             (keyword? info) :spec
                             (vector?  info) :inline)))

(defmethod transform-enum :spec
  [spec]
  [(csk/->PascalCaseKeyword spec)
   (-> {:values (mapv csk/->SCREAMING_SNAKE_CASE_KEYWORD (get-enum-values spec))}
       (-assoc-description (get-enum-description spec)))])

(defmethod transform-enum :inline
  [[key values description]]

  [(csk/->PascalCaseKeyword key)
   (-> {:values (mapv csk/->SCREAMING_SNAKE_CASE_KEYWORD values)}
       (-assoc-description description))])

(defmulti transform-object (fn [record]
                             (cond
                               (contains? record :entity) :entity
                               (contains? record :object) :object
                               :else (throw (ex-info "Can't transform object" record)))))

(defmethod transform-object :entity
  [{:keys [entity] :as record}]
  [(csk/->PascalCaseKeyword entity)
   (-> {:fields (map (fn [{:keys [data-type description] :as record}]
                       (-> {:type data-type}
                           (-assoc-description description)))
                     (get-entity-fields entity))}
       (-assoc-description (get-entity-description entity)))])

(defmethod transform-object :object
  [{:keys [object fields description] :as record}]
  [(csk/->PascalCaseKeyword object)
   (-> {:fields (map (fn [[key type description]]
                       [(csk/->camelCaseKeyword key)
                        (-> {:type type}
                            (-assoc-description description))])
                     fields)}
       (-assoc-description description))])
