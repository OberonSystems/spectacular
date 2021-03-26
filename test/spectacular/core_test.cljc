(ns spectacular.core-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]])
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

(register-field ::name           ::string)

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
                 ::sp/required-keys [::guitar-brand ::name ::pickup-1]
                 ::sp/fields        {::name #::sp{:label       "Guitar Name"
                                                  :description "BB King called his 'Lucille'."}})

(register-entity ::player           [::name ::age]
                 ::sp/identity-keys [::name]
                 ::sp/required-keys [::age]
                 ::sp/fields        {::name {::sp/label       "Player Name"
                                             ::sp/description "Usually something cool."}})

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

(deftest gql-transform-fields
  (is (= (-> {:field-key :players
              :type      :player
              :list?     true
              :required? true
              :resolve   'get-guitar-players}
             gql/gql-field->field
             gql/transform-field)
         [:players
          {:type '(non-null (list (non-null Player)))
           :resolve 'get-guitar-players}]))

  (is (= (gql/transform-field {:field-key   :wildcard
                               :type        :string
                               :description "Use * for wildcard"})
         [:wildcard {:type 'String :description "Use * for wildcard"}]))

  (is (= (-> {:field-key ::guitar-brand}
             gql/gql-field->field
             gql/transform-field)
         [:guitarBrand {:type 'GuitarBrand
                        :description "A small selection of Guitar Brands"}]))

  (is (= (-> {:field-key ::guitar :required? true}
             gql/gql-field->field
             gql/transform-field)
         [:guitar {:type '(non-null Guitar)
                   :description "Traditionally a 6 stringed instrument."}])))

(deftest gql-schema-objects
  (is (= (gql/make-schema {:objects {:guitar-token {:object-type :entity-token :entity-key ::guitar}
                                     :guitar       {:object-type :entity       :entity-key ::guitar}
                                     ;;
                                     :pick {:object-type :graphql
                                            :fields      {:colour {:type :string :required? true}
                                                          :brand  {:type :string :required? true :description "Brand of Pick"}}
                                            :description "Some like em thick and others like em thin."}}})
         {:objects {:GuitarToken {:fields {:guitarUuid
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
                                  :description "Some like em thick and others like em thin."}}}))
  ;;
  (is (= (gql/make-schema {:objects {:guitar {:object-type :entity
                                              :entity-key  ::guitar
                                              :fields      {:players {:type      ::player
                                                                      :required? true
                                                                      :list?     true
                                                                      :resolve   'fetch-guitar-players}}}}})
         {:objects {:Guitar {:fields {:guitarUuid  {:type 'WeirdoUuid  :description "Globally Unique ID."}
                                      :guitarBrand {:type 'GuitarBrand :description "A small selection of Guitar Brands"}
                                      :name        {:type 'String  :description "BB King called his 'Lucille'."}
                                      :age         {:type 'Integer :description "Age in years."}
                                      :pickup1     {:type 'Pickup1}
                                      :pickup2     {:type 'Pickup2}
                                      :pickup3     {:type 'Pickup3}
                                      :players     {:type '(list (non-null Player)) :resolve 'fetch-guitar-players}}
                             :description "Traditionally a 6 stringed instrument."}}})))

(deftest gql-schema-input-objects
  (is (= (gql/make-schema {:input-objects {:guitar-token-in {:object-type :entity-token :entity-key ::guitar}}})
         {:input-objects
          {:GuitarTokenIn {:fields {:guitarUuid {:type        '(non-null WeirdoUuid)
                                                 :description "Globally Unique ID."}}
                           :description "Traditionally a 6 stringed instrument."}}}))

  (is (= (gql/make-schema {:input-objects {:guitar-content-in {:object-type :entity-content :entity-key ::guitar}}})
         {:input-objects {:GuitarContentIn
                          {:fields {:guitarBrand {:type '(non-null GuitarBrand)
                                                  :description "A small selection of Guitar Brands"}
                                    :name        {:type '(non-null String)
                                                  :description "BB King called his 'Lucille'."}
                                    :age         {:type 'Integer :description "Age in years."}
                                    :pickup1     {:type '(non-null Pickup1)}
                                    :pickup2     {:type 'Pickup2}
                                    :pickup3     {:type 'Pickup3}}
                           :description "Traditionally a 6 stringed instrument."}}}))

  (is (= (gql/make-schema {:input-objects {:guitar-in {:object-type :entity :entity-key ::guitar}}})
         {:input-objects {:GuitarIn {:fields {:guitarUuid  {:type '(non-null WeirdoUuid)  :description "Globally Unique ID."}
                                              :guitarBrand {:type '(non-null GuitarBrand) :description "A small selection of Guitar Brands"}
                                              :name        {:type '(non-null String)      :description "BB King called his 'Lucille'."}
                                              :age         {:type 'Integer                :description "Age in years."}
                                              :pickup1     {:type '(non-null Pickup1)}
                                              :pickup2     {:type 'Pickup2}
                                              :pickup3     {:type 'Pickup3}}
                                     :description "Traditionally a 6 stringed instrument."}}})))

(deftest gql-schema-queries
  (is (= (gql/make-schema {:objects (merge gql/+page-object+
                                           {:paged-guitars (gql/paged ::guitar)})
                           :queries {:fetch-guitars {:args        {:guitar-brand {:type ::guitar-brand}
                                                                   :wildcard     {:type :string :description "Wild card search for guitar."}
                                                                   :page         {:type ::gql/page}}
                                                     :type        :paged-guitars
                                                     :resolve     'fetch-guitars}
                                     :fetch-guitar  {:args        {:token {:type ::guitar-token :required? true}}
                                                     :type        ::guitar
                                                     :description "Fetches a single guitar"
                                                     :resolve     'fetch-guitar}}})
         {:objects {:Page         {:fields {:index {:type '(non-null Int) :description "Zero based index of page."}
                                            :size  {:type '(non-null Int) :description "Max number of records to include in each page."}}}
                    :PagedGuitars {:fields {:total   {:type '(non-null Int) :description "Total number of matched results."}
                                            :records {:type '(non-null (list (non-null Guitar)))}
                                            :page    {:type 'Page}}}}
          :queries {:fetchGuitars {:type 'PagedGuitars
                                   :args {:guitarBrand {:type 'GuitarBrand :description "A small selection of Guitar Brands"}
                                          :wildcard    {:type 'String :description "Wild card search for guitar."}
                                          :page        {:type 'Page}}
                                   :resolve 'fetch-guitars}
                    :fetchGuitar {:type 'Guitar
                                  :args {:token {:type '(non-null GuitarToken)}}
                                  :resolve 'fetch-guitar
                                  :description "Fetches a single guitar"}}})))

(deftest gql-schema-mutations
  (is (= (gql/make-schema {:mutations {:add-guitar    {:args    {:record {:type ::guitar-in :required? true}}
                                                       :type    ::guitar
                                                       :resolve 'add-guitar}
                                       :modify-guitar {:args    {:token   {:type ::guitar-token-in   :required? true}
                                                                 :content {:type ::guitar-content-in :required? true}}
                                                       :type    ::guitar
                                                       :resolve 'modify-guitar}
                                       :remove-guitar {:args    {:token {:type ::guitar-token-in   :required? true}}
                                                       :type    :boolean
                                                       :resolve 'remove-guitar}}})
         {:mutations {:addGuitar    {:type    'Guitar
                                     :args    {:record {:type '(non-null GuitarIn)}}
                                     :resolve 'add-guitar}
                      :modifyGuitar {:type    'Guitar
                                     :args     {:token {:type '(non-null GuitarTokenIn)}
                                                :content {:type '(non-null GuitarContentIn)}}
                                     :resolve  'modify-guitar}
                      :removeGuitar {:type    'Boolean
                                     :args    {:token {:type '(non-null GuitarTokenIn)}}
                                     :resolve 'remove-guitar}}})))
