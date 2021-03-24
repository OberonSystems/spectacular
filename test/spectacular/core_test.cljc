(ns spectacular.core-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.data :refer [diff]])
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

(register-enum ::pickup-brand [::fender ::gibson ::lolar])

;;;

(register-field ::pickup-brand ::pickup-brand
                ::sp/label "Brand of Pickup")

(register-field ::pickup-position ::integer
                ::sp/label       "Position"
                ::sp/description "Position, 1, 2 or 3 for Strats, 1 or 2 for Teles, etc.")

(register-entity ::pickup-1 [::pickup-brand ::pickup-position])
(clone-entity    ::pickup-2 ::pickup-1)
(clone-entity    ::pickup-3 ::pickup-1)

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

(register-entity ::guitar [::guitar-uuid
                           ::guitar-brand
                           ::name
                           ::age
                           ::pickup-1
                           ::pickup-2
                           ::pickup-3]
                 ;;
                 ::sp/description "Traditionally a 6 stringed instrument."
                 ;;
                 ::sp/identity-keys [::guitar-uuid]
                 ::sp/optional-keys [::age ::pickup-2 ::pickup-3]
                 ::sp/fields        {::name #::sp{:label       "Guitar Name"
                                                  :description "BB King called his 'Lucille'."}})

(deftest test-scalars
  (is (= (get-scalar-description ::string) "Non Blank String"))
  (is      (s/valid? ::string "blah"))
  (is (not (s/valid? ::string 10))))

(deftest test-entities
  (is (s/valid? ::guitar {::guitar-uuid  #uuid "2f67ecd2-196b-4cc8-8b43-4b0d11d614f9"
                          ::guitar-brand ::gibson
                          ::name         "Lucille"
                          ::age          10
                          ::pickup-1     {::pickup-brand    ::fender
                                          ::pickup-position 1}}))

  (is (s/valid? ::guitar {::guitar-uuid  #uuid "2f67ecd2-196b-4cc8-8b43-4b0d11d614f9"
                          ::guitar-brand ::gibson
                          ::name         "Lucille"
                          ::age          10
                          ::pickup-1     {::pickup-brand    ::fender
                                          ::pickup-position 1}
                          ::pickup-2     {::pickup-brand    ::lolar
                                          ::pickup-position 2}}))

  (-> (s/valid? ::guitar {::guitar-uuid  #uuid "2f67ecd2-196b-4cc8-8b43-4b0d11d614f9"
                          ::guitar-brand ::gibson
                          ::name         "Lucille"
                          ::age          10
                          ::pickup-1     {::pickup-brand    ::fender
                                          ::pickup-position 1}
                          ::pickup-2     {::pickup-brand    ::lolar}})
      not
      is))

;;; --------------------------------------------------------------------------------
;;  GraphQL Tests

(deftest gql-schema-enums
  (is (= (get-enum-labels ::guitar-brand)
         [[::fender "Fender"]
          [::gibson "Gibson"]
          [::prs    "Paul Reed Smith"]
          [::martin "Martin"]
          [::gretch "Gretch"]]))
  (is (s/valid? ::guitar-brand ::fender))
  (is (not (s/valid? ::guitar-brand ::something-random)))
  ;;
  (is (= (gql/make-schema {:enums [::guitar-brand
                                   [:just-for-graphql-1 [:graph1 :graph2 :graph3] "A Description"]
                                   [:just-for-graphql-2 [:graph1 :graph2 :graph3]]]})
         {:enums
          {:GuitarBrand {:values [:FENDER :GIBSON :PRS :MARTIN :GRETCH]}
           :JustForGraphql1
           {:values [:GRAPH_1 :GRAPH_2 :GRAPH_3]
            :description "A Description"}
           :JustForGraphql2 {:values [:GRAPH_1 :GRAPH_2 :GRAPH_3]}}})))

(deftest gql-schema-objects
  (is (= (gql/make-schema {:objects {:guitar-token {:object-type :entity-token :entity-key ::guitar}
                                     :guitar       {:object-type :entity       :entity-key ::guitar}
                                     ;;
                                     :pick {:object-type :graphql
                                            :fields      {:colour {:gql-type :string}
                                                          :brand  {:gql-type :string :description "Brand of Pick"}}
                                            :description "Some like em thick and others like em thin."}}})
         {:objects {:GuitarToken {:fields
                                  {:guitarUuid
                                   {:type '(non-null WeirdoUuid)
                                    :description "Globally Unique ID."}}}
                    :Guitar      {:fields
                                  {:guitarUuid  {:type 'WeirdoUuid :description "Globally Unique ID."}
                                   :guitarBrand {:type 'GuitarBrand
                                                 :description "A small selection of Guitar Brands"}
                                   :name {:type 'String :description "BB King called his 'Lucille'."}
                                   :age  {:type 'Integer :description "Age in years."}
                                   :pickup1 {:type 'Pickup1}
                                   :pickup2 {:type 'Pickup2}
                                   :pickup3 {:type 'Pickup3}}
                                  :description "Traditionally a 6 stringed instrument."}
                    :Pick        {:fields
                                  {:colour {:type '(non-null String)}
                                   :brand  {:type '(non-null String) :description "Brand of Pick"}}
                                  :description "Some like em thick and others like em thin."}}})))

(deftest gql-schema-input-objects
  (is (= (gql/make-schema {:input-objects {:guitar-token-in   {:object-type :entity-token   :entity-key ::guitar}
                                           :guitar-content-in {:object-type :entity-content :entity-key ::guitar}
                                           :guitar-in         {:object-type :entity         :entity-key ::guitar}}})
         {:input-objects
          {:GuitarTokenIn {:fields {:guitarUuid {:type '(non-null WeirdoUuid)}}
                           :description "Traditionally a 6 stringed instrument."}
           :GuitarContentIn {:fields {:guitarBrand {:type '(non-null GuitarBrand)}
                                      :name        {:type '(non-null String)}
                                      :age         {:type 'Integer}
                                      :pickup1     {:type '(non-null Pickup1)}
                                      :pickup2     {:type 'Pickup2}
                                      :pickup3     {:type 'Pickup3}}
                             :description "Traditionally a 6 stringed instrument."}
           :GuitarIn {:fields {:guitarUuid  {:type '(non-null WeirdoUuid)}
                       :guitarBrand {:type '(non-null GuitarBrand)}
                       :name        {:type '(non-null String)}
                       :age         {:type 'Integer}
                       :pickup1     {:type '(non-null Pickup1)}
                       :pickup2     {:type 'Pickup2}
                       :pickup3     {:type 'Pickup3}}
                      :description "Traditionally a 6 stringed instrument."}}})))

#_
(deftest gql-transform-args
  (is (= (gql/transform-arg :wildcard {:arg-type :graphql :gql-type :string :description "Use * for wildcard"})
         [:wildcard {:type '(non-null String) :description "Use * for wildcard"}]))

  (is (= (gql/transform-query-arg {:arg-key ::guitar-brand})
         [:guitarBrand
          {:type 'GuitarBrand
           :description "A small selection of Guitar Brands"}]))

  (is (= (gql/transform-query-arg {:arg-key ::guitar :required? true})
         [:guitar
          {:type '(non-null Guitar)
           :description "Traditionally a 6 stringed instrument."}])))

(deftest gql-schema-queries
  (is (= (gql/make-schema {:queries {:fetch-guitars {:args        {:guitar-brand {:arg-type ::guitar-brand}
                                                                   :wildcard     {:arg-type    :string
                                                                                  :description "Wild card search for guitar."}
                                                                   :page         {:arg-type ::gql/page}}
                                                     :return-type ::paged-guitars
                                                     :resolve     'fetch-guitars}
                                     :fetch-guitar  {:args        {:token {:arg-type  ::guitar-token
                                                                           :required? true}}
                                                     :return-type ::guitar
                                                     :description "Fetches a single guitar"
                                                     :resolve     'fetch-guitar}}})
         {:queries {:fetchGuitars {:type 'PagedGuitars
                                   :args {:guitarBrand {:type 'GuitarBrand :description "A small selection of Guitar Brands"}
                                          :wildcard    {:type 'String :description "Wild card search for guitar."}
                                          :page        {:type 'Page}}
                                   :resolve 'fetch-guitars}
                    :fetchGuitar
                    {:type 'Guitar
                     :args {:token {:type '(non-null GuitarToken)}}
                     :resolve 'fetch-guitar
                     :description "Fetches a single guitar"}}})))
