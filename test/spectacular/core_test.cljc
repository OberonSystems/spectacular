(ns spectacular.core-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s])
  (:require [spectacular.core    :refer :all :as sp]
            [spectacular.graphql :as gql]
            :reload))

;;; Some globals to then test with

(register-scalar ::string     string?
                 ::sp/pg-type ::text
                 ;;
                 ::sp/description "Non Blank String")

(register-scalar ::integer int?)

(register-scalar ::uuid uuid?)

;;;

(register-enum ::guitar-brand [::fender ::gibson ::prs ::martin ::gretch]
               ;;
               ::sp/labels      {::prs "Paul Reed Smith"}
               ::sp/description "A small selection of Guitar Brands")

;;;

(register-field ::guitar-uuid    ::uuid
                ::sp/label       "Guitar ID"
                ::sp/description "Globally Unique ID."
                ::sp/gql-type    :weirdo-uuid)

(register-field ::name           ::string
                ::sp/label       "Guitar"
                ::sp/description "Usually a six stringed instrument.")

(register-field ::guitar-brand ::guitar-brand
                ::sp/label     "Guitar Brand")

(register-field ::age            ::integer
                ::sp/label       "Age"
                ::sp/description "Age in years.")

(register-entity ::guitar [::guitar-uuid ::guitar-brand ::name ::age]
                 ;;
                 ::sp/description "Traditionally a 6 stringed instrument."
                 ;;
                 ::sp/identity-keys [::guitar-uuid]
                 ::sp/optional-keys [::age]
                 ::sp/fields        {::name #::sp{:label       "Guitar Name"
                                                  :description "BB King called his 'Lucille'."}})

(def +test-schema+
  {:enums   [::guitar-brand
             ;;
             ;; Or declare inline
             [:just-for-graphql-1 [:graph1 :graph2 :graph3] "A Description"]
             [:just-for-graphql-2 [:graph1 :graph2 :graph3]]]
   ;;
   :objects [{:object-type :entity-token :entity-key ::guitar}
             {:object-type :entity       :entity-key ::guitar}
             ;;
             ;; Can also declare inline if need be, but you don't get
             ;; any of the other smarts like auto Input objects, etc.
             {:object-type :graphql
              :graphql-key :pick
              :description "Some like em thick and others like em thin."
              :fields      [[:colour 'String "Colour"]
                            [:brand  '(non-null String) "Brand of Pick"]]}]
   ;;
   :queries [:fetch-guitars {:type    [::guitar]
                             :args    [::brand]
                             :resolve 'fetch-guitars}
             ;;
             :fetch-guitar  {:type    ::guitar
                             :args    [::guitar-uuid
                                       {:graphql-key :brand-like
                                        :type        :string
                                        :description "Wild card search for guitar."}]
                             :resolve 'fetch-guitar}]
   ;;
   :input-objects [{:object-type :entity-token   :entity-key ::guitar}
                   {:object-type :entity-content :entity-key ::guitar}]
   :mutations     {:add-guitar    {:type ::guitar
                                   :args [:guitar-input-add]
                                   :resolve '(add-entity ::guitar %)}
                   :modify-guitar {:type ::guitar
                                   :args [:guitar-input-modify]
                                   :resolve '(modify-entity ::guitar %)}
                   :remove-guitar {:type 'boolean
                                   :args [:guitar-input-token]}}})

(deftest test-scalars
  (is (= (get-scalar-description ::string) "Non Blank String"))
  (is      (s/valid? ::string "blah"))
  (is (not (s/valid? ::string 10))))

(deftest test-enums
  (is (= (get-enum-labels ::guitar-brand)
         [[::fender "Fender"]
          [::gibson "Gibson"]
          [::prs    "Paul Reed Smith"]
          [::martin "Martin"]
          [::gretch "Gretch"]]))
  (is (s/valid? ::guitar-brand ::fender))
  (is (not (s/valid? ::guitar-brand ::something-random))))

(deftest test-entities
  (is (s/valid? ::guitar {::guitar-uuid  #uuid "2f67ecd2-196b-4cc8-8b43-4b0d11d614f9"
                          ::guitar-brand ::gibson
                          ::name         "Lucille"
                          ::age          10})))

(deftest test-graphql
  (is (= (mapv gql/transform-enum (+test-schema+ :enums))
         [[:GuitarBrand {:values [:FENDER :GIBSON :PRS :MARTIN :GRETCH]}]
          [:JustForGraphql1
           {:values [:GRAPH_1 :GRAPH_2 :GRAPH_3]
            :description "A Description"}]
          [:JustForGraphql2 {:values [:GRAPH_1 :GRAPH_2 :GRAPH_3]}]]))

  (is (= (mapv gql/transform-object (+test-schema+ :objects))
         [[:GuitarToken
           {:fields
            {:guitarUuid
             {:type '(non-null WeirdoUuid)
              :description "Globally Unique ID."}}}]
          [:Guitar
           {:fields
            {:guitarUuid
             {:type 'WeirdoUuid :description "Globally Unique ID."}
             :guitarBrand
             {:type 'GuitarBrand
              :description "A small selection of Guitar Brands"}
             :name
             {:type 'String :description "BB King called his 'Lucille'."}
             :age {:type 'Integer :description "Age in years."}}
            :description "Traditionally a 6 stringed instrument."}]
          [:Pick
           {:fields
            {:colour {:type 'String :description "Colour"}
             :brand {:type '(non-null String) :description "Brand of Pick"}}
            :description "Some like em thick and others like em thin."}]]))

  (is (= (mapv gql/transform-input-object (+test-schema+ :input-objects))
         [[:GuitarInToken
           {:fields {:guitarUuid {:type '(non-null WeirdoUuid)}}
            :description "Traditionally a 6 stringed instrument."}]
          [:GuitarIn
           {:fields
            {:guitarBrand {:type '(non-null GuitarBrand)}
             :name {:type '(non-null String)}
             :age {:type 'Integer}}
            :description "Traditionally a 6 stringed instrument."}]])))
