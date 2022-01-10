(ns spectacular.lacinia-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]])
  (:require [spectacular.core    :as sp]
            [spectacular.lacinia :as lc]
            :reload))

(defn java-util-date?
  [d]
  (instance? java.util.Date d))

;; Some common scalars that we'll use in subsequent tests
(sp/scalar :scalar/string  string? ::sp/description "Non Blank String")
(sp/scalar :scalar/boolean boolean?)
(sp/scalar :scalar/integer integer?)
(sp/scalar :scalar/ju-date java-util-date? ::sp/description "Java Date")

;;; --------------------------------------------------------------------------------

(sp/attribute :test/user-id     :scalar/string)
(sp/attribute :test/given-name  :scalar/string)
(sp/attribute :test/family-name :scalar/string)
(sp/attribute :test/dob         :scalar/ju-date)
(sp/attribute :test/height      :scalar/integer)
(sp/attribute :test/citizen?    :scalar/boolean)

(sp/entity :test/user
           [:test/user-id
            :test/given-name
            :test/family-name
            :test/dob
            :test/height
            :test/citizen?]
           ::sp/identity-keys [:test/user-id]
           ::sp/required-keys [:test/family-name])

(sp/enum      :test/user-role-type [:one :two :three])
(sp/attribute :test/user-role      :test/user-role-type)

(sp/entity :test/user-role
           [:test/user-id
            :test/user-role]
           ::sp/identity-keys [:test/user-id :test/user-role])

(sp/entity-token  :test/user-token  :test/user)
(sp/entity-values :test/user-values :test/user)

;;; --------------------------------------------------------------------------------

(deftest gql-type-names
  (are [lhs rhs] (= lhs (apply lc/gql-type-name rhs))
    :AddressIn [:ab/address {:input? true}]
    :Address   [:ab/address nil]
    ;;
    :UserRole   [:test/user-role nil]
    :UserRoleIn [:test/user-role {:input? true}]
    ;;
    :User   [:test/user nil]
    :UserIn [:test/user {:input? true}]
    ;;
    :UserToken   [:test/user-token nil]
    :UserTokenIn [:test/user-token {:input? true}]
    ;;
    :UserValues   [:test/user-values nil]
    :UserValuesIn [:test/user-values {:input? true}]))

;;; --------------------------------------------------------------------------------

(deftest refs->fields
  (is (= (lc/ref->field :scalar/string)
         {:type        :String
          :description "Non Blank String"}))

  (is (= (lc/ref->field :scalar/string)
         {:type        :String
          :description "Non Blank String"}))

  (is (= (lc/ref->field {:type        :scalar/string
                         :description "Something Else"})
         {:type        :String
          :description "Something Else"}))

  (is (= (lc/ref->field {:type        :scalar/string
                         :description "Something Else"
                         ;;
                         ::lc/type   :strange-string})
         {:type        :StrangeString
          :description "Something Else"}))

  (is (= (lc/ref->field :test/user-id)
         {:type :String}))

  (is (= (lc/ref->field {:type        :test/user-id
                         :description "User ID"})
         {:type        :String
          :description "User ID"}))

  (is (= (lc/ref->field {:type        :test/user-id
                         :required?   true})
         {:type        '(non-null :String)}))
  ;;
  ;; Handling lists
  (is (= (lc/ref->field {:type [:scalar/string]})
         {:type        '(list (non-null :String))
          :description "Non Blank String"})))

;;; --------------------------------------------------------------------------------

(sp/enum   :scalar/au-state-1 [:act :nsw :nt :qld :sa :tas :vic :wa]
           ;;
           ::sp/labels  {:qld "Queensland"
                         :nsw "New South Wales"
                         :act "Australian Capital Territory"
                         :vic "Victoria"
                         :tas "Tasmania"
                         :nt  "Northern Territory"
                         :sa  "South Australia"
                         :wa  "Western Australia"}
           ::sp/abbrevs {:qld "QLD"
                         :nsw "NSW"
                         :act "ACT"
                         :vic "VIC"
                         :tas "TAS"
                         :nt  "NT"
                         :sa  "SA"
                         :wa  "WA"}
           ::sp/description "An Australian State or Territory")

(sp/enum :scalar/au-state-2 [:act :nsw :nt :qld :sa :tas :vic :wa]
         ::sp/description "An Australian State or Territory"
         ::lc/description "An Australian State or Territory for GQL.")

(sp/enum :scalar/au-state-3 [:act :nsw :nt :qld :sa :tas :vic :wa]
         ::sp/description  "An Australian State or Territory"
         ::lc/type        :aus-state
         ::lc/description "An Australian State or Territory for GQL, with a different key.")

(deftest types->enums
  (testing "Inline Transforms"
    (is (= (lc/enum->enum {:type   :test
                           :values [:this :is :values]
                           :description "What evs"})
           [:Test
            {:values      [:THIS :IS :VALUES]
             :description "What evs"}])))

  (testing "SP Transforms"
    (is (= (lc/enum->enum :scalar/au-state-1)
           [:AuState1
            {:values [:ACT :NSW :NT :QLD :SA :TAS :VIC :WA],
             :description "An Australian State or Territory"}]))

    (is (= (lc/enum->enum :scalar/au-state-2)
           [:AuState2
            {:values [:ACT :NSW :NT :QLD :SA :TAS :VIC :WA],
             :description "An Australian State or Territory for GQL."}]))

    (is (= (lc/enum->enum :scalar/au-state-3)
           [:AusState
            {:values [:ACT :NSW :NT :QLD :SA :TAS :VIC :WA],
             :description "An Australian State or Territory for GQL, with a different key."}])))

  (testing "Can't Transform"
    (is (thrown? clojure.lang.ExceptionInfo
                 (lc/enum->enum :invalid)))))

(deftest entities->output-fields
  (is (= (lc/entity->output-fields :test/user)
         {:userId     {:type :String}
          :givenName  {:type :String}
          :familyName {:type :String}
          :dob        {:type :JuDate}
          :height     {:type :Integer}
          :isCitizen  {:type :Boolean}}))

  (is (= (lc/entity->output-fields :test/user-token)
         {:userId     {:type :String}}))

  (is (= (lc/entity->output-fields :test/user-values)
         {:givenName  {:type :String}
          :familyName {:type :String}
          :dob        {:type :JuDate}
          :height     {:type :Integer}
          :isCitizen  {:type :Boolean}}))

  (is (= (lc/entity->output-fields :test/user-role)
         {:userId   {:type :String}
          :userRole {:type :UserRole}}))

  (is (= (lc/entity->output-fields {:test-id :scalar/string
                                    :age     :scalar/integer
                                    :uuid    :scalar/string})
         {:testId {:type :String, :description "Non Blank String"}
          :age    {:type :Integer}
          :uuid   {:type :String :description "Non Blank String"}}))

  (is (= (lc/entity->output-fields {:test-id :scalar/string
                                    :age     :scalar/integer
                                    :uuid    {:type     :scalar/string
                                              :resolver 'test-uuid-resolver}})
         {:testId {:type :String, :description "Non Blank String"}
          :age    {:type :Integer}
          :uuid   {:type        :String
                   :description "Non Blank String"
                   :resolver    'test-uuid-resolver}})))

(deftest entities->input-fields
  (is (= (lc/entity->input-fields :test/user)
         {:userId     {:type '(non-null :String)}
          :givenName  {:type :String}
          :familyName {:type '(non-null :String)}
          :dob        {:type :JuDate}
          :height     {:type :Integer}
          :isCitizen  {:type :Boolean}}))

  (is (= (lc/entity->input-fields :test/user-token)
         {:userId {:type '(non-null :String)}}))

  (is (= (lc/entity->input-fields :test/user-values)
         {:givenName  {:type :String},
          :familyName {:type '(non-null :String)},
          :dob        {:type :JuDate},
          :height     {:type :Integer},
          :isCitizen  {:type :Boolean}}))

  (is (= (lc/entity->input-fields {:givenName  {:type :string}
                                   :familyName {:type :string :required? true}
                                   :dob        {:type :ju-date}
                                   :height     {:type :integer}
                                   :citizen?   {:type :boolean}})
         {:givenName  {:type :String}
          :familyName {:type '(non-null :String)}
          :dob        {:type :JuDate}
          :height     {:type :Integer}
          :isCitizen  {:type :Boolean}})))

(sp/enum :test/enum-type-1 [:one  :two  :three])
(sp/enum :test/enum-type-2 [:four :five :six])

(sp/attribute :test/enum-1 :test/enum-type-1)
(sp/attribute :test/enum-2 :test/enum-type-2)
(sp/entity    :test/grand-child-entity [:test/enum-1 :test/enum-2])

(sp/attribute :test/grand-child-attr :test/grand-child-entity)
(sp/entity    :test/child-entity     [:test/grand-child-attr])

(sp/attribute :test/child-attr    :test/child-entity)
(sp/entity    :test/parent-entity [:test/child-attr])

(deftest referenced-entities
  (is (= (lc/referenced-entities :test/parent-entity)
         #{:test/grand-child-entity :test/child-entity}))

  (is (= (lc/referenced-enums :test/grand-child-entity)
         #{:test/enum-type-1 :test/enum-type-2})))

(deftest endpoints->gql
  (is (= (lc/endpoint->gql {:type     :test/user
                            :args     {:user-id :test/user-id}
                            :resolver 'fetch-user})
         '{:type     :User
           :args     {:userId {:type :String}}
           :resolver fetch-user}))

  (is (= (lc/endpoint->gql {:type [:test/user]
                            :args {:name-like {:type :string}
                                   :tags      [:string]}})
         '{:type (list (non-null :User))
           :args {:nameLike {:type :String}
                  :tags     {:type (list (non-null :String))}}}))

  (is (= (lc/endpoint->gql {:type [:test/user]
                            :args {:name-like {:type      :string
                                               :required? true}
                                   :tags      [:string]}})
         '{:type (list (non-null :User))
           :args {:nameLike {:type (non-null :String)}
                  :tags     {:type (list (non-null :String))}}}))

  (is (= (lc/endpoint->gql {:type [:test/user]
                            :args {:name-like {:type      :string
                                               :required? true}
                                   :tags      [:string]}
                            :description "DESCRIPTION"
                            :resolver    'resolve-me})
         '{:type        (list (non-null :User))
           :args        {:nameLike {:type (non-null :String)}
                         :tags     {:type (list (non-null :String))}}
           :description "DESCRIPTION"
           :resolver    resolve-me}))

  (is (= (lc/endpoint->gql {:type {:type      :boolean
                                   :required? true}})
         '{:type (non-null :Boolean)}))

  (is (= (lc/endpoint->gql {:type     [:test/user]
                            :args     {:token {:type      :test/user
                                               :token?    true
                                               :required? true}}
                            :resolver 'fetch-user})
         '{:type     (list (non-null :User))
           :args     {:token {:type (non-null :UserTokenIn)}}
           :resolver fetch-user})))

(deftest endpoints->gql
  (let [q1 {:fetch-user {:type     [:test/user]
                         :args     {:user-id {:type      :test/user-id
                                              :required? true}}
                         :resolver 'fetch-user}}

        q2 {:fetch-user {:type     [:test/user]
                         :args     {:token {:type      :test/user
                                            :token?    true
                                            :required? true}}
                         :resolver 'fetch-user}}]
    (is (= (lc/endpoints->gql q1)
           [[:fetchUser '{:type     (list (non-null :User)),
                          :args     {:userId {:type (non-null :String)}},
                          :resolver fetch-user}]]))))

(def +q1+ {:fetch-user       {:type     :test/user
                              :args     {:token {:type      :test/user-token
                                                 :required? true}}
                              :resolver 'fetch-user}
           :fetch-user-by-id {:type     :test/user
                              :args     {:user-id :string}
                              :resolver 'fetch-user}
           :fetch-users      {:type     [:test/user]
                              :resolver 'fetch-user}})

(def +m1+ {:add-user    {:type :test/user
                         :args {:record {:type      :test/user-values
                                         :required? true}}}
           :modify-user {:type :test/user
                         :args {:record {:type      :test/user
                                         :required? true}}}
           :remove-user {:type :boolean
                         :args {:record {:type      :test/user-token
                                         :required? true}}}})

(deftest endpoint-input-and-output-types
  (is (= (lc/endpoint-output-objects +q1+ +m1+)
         [{:type :test/user :object-type :entity}]))

  (is (= (lc/endpoint-input-objects +q1+ +m1+)
         [{:type :test/user        :object-type :entity}
          {:type :test/user-token  :object-type :entity}
          {:type :test/user-values :object-type :entity}])))

#_
(deftest output-input-objects

  (let [output-types (lc/endpoint-output-types +q1+ +m1+)
        input-types  (lc/endpoint-input-types +q1+ +m1+)]
    (pprint (map lc/entity->output-fields output-types))
    )

  )

#_
(deftest transform-schema

  {:objects
   ;; This will expand into the User object based on the entity and
   ;; will include an additional 'roles' field that is a resolver.
   {:user {:entity    :ab/user
           :resolvers {:roles {:type {:entity [:ab/role]}}}}}

   :queries
   ;; This will use the 'User' created above, it was already created
   ;; it would add it to the objects.
   {:fetch-users      {:type [:ab/user]
                       :args {:name-like {:type :string}
                              :tags      {:type [:string]}}}

    :fetch-user-roles {:type   [:ab/roles]
                       :args {:user {:entity    :ab/user
                                     ;; UserToken
                                     :token?    true
                                     :required? true}}}}
   :mutations
   {:add-user    {:type     :ab/user
                  :args     {:record {:entity    :ab/user
                                      ;; UserValuesIn
                                      :values?   true
                                      :required? true}}}
    :remove-user {:type     :ab/user
                  :args     {:record {:entity    :ab/user
                                      ;; UserTokenIn
                                      :token?    true
                                      :required? true}}}
    :modify-user {:type     :ab/user
                  :args     {:record {:entity    :ab/user
                                      ;; UserIn
                                      :required? true}}}}}




  ;; Should get transformed into;
  {:objects       {:user    {:fields {:user-id     {:type :string}
                                      :given-name  {:type :string}
                                      :family-name {:type :string}}}}

   :input-objects {:user-in {:fields {:given-name  {:type :string}
                                      :family-name {:type      :string
                                                    :required? true}}}}
   ;;
   :queries   {:fetch-users {:type     '(list (not-null :user))
                             :fields   {:name-like {:type :string}
                                        :tags      {:type '(list :string)}}
                             :resolver 'identity}}
   :mutations {:modify-user {:type     :user
                             :args     {:record {:type (not-null :user-in)}}
                             :resolver 'identity}}})
