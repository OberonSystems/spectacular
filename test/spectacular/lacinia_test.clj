(ns spectacular.lacinia-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]])
  (:require [spectacular.core    :as sp]
            [spectacular.lacinia :as lc]
            :reload))

;; Ensure the registry gets cleared when we recompile.
(sp/clear!)

(defn java-util-date?
  [d]
  (instance? java.util.Date d))

;; Some common scalars that we'll use in subsequent tests
(sp/scalar :scalar/string   string? ::sp/description "Non Blank String")
(sp/scalar :scalar/string-2 string?
           ::sp/description "Non Blank String"
           ;;
           ::lc/type        :strange-string
           ::lc/description "Like a String but stranger.")
(sp/scalar :scalar/boolean boolean?)
(sp/scalar :scalar/integer integer?)
(sp/scalar :scalar/ju-date java-util-date? ::sp/description "Java Date")

;;; --------------------------------------------------------------------------------

(sp/attribute :test/user-id     :scalar/string)
(sp/attribute :test/given-name  :scalar/string)
(sp/attribute :test/family-name :scalar/string)
(sp/attribute :test/dob         :scalar/ju-date)
(sp/attribute :test/height      :scalar/integer)
(sp/attribute :test/citizen?    :scalar/boolean
              ::lc/name :is-citizen)

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
(sp/attribute :test/user-role-attr :test/user-role-type)

(sp/entity :test/user-role
           [:test/user-id
            :test/user-role-attr]
           ::sp/identity-keys [:test/user-id :test/user-role-attr]
           ;;
           ::lc/description   "Links a user to a Role they can perform.")

(sp/entity-token  :test/user-token  :test/user)
(sp/entity-values :test/user-values :test/user)

;;; --------------------------------------------------------------------------------

(deftest refs->field-types
  ;; Add some tests so that only entities get the In suffix.
  ;;
  ;; Also ensure one of the entities has a different :lc/type and
  ;; check that.
  (are [lhs rhs] (= lhs (let [ref (-> rhs first lc/canonicalise-ref)
                              in? (-> rhs second)]
                          (lc/ref->field-type ref :in? in?)))
    :StrangeString [:scalar/string-2 nil]
    :StrangeString [:scalar/string-2 true]

    :UserRole   [:test/user-role nil]
    :UserRoleIn [:test/user-role true]
    ;;
    :User   [:test/user nil]
    :UserIn [:test/user true]
    ;;
    :UserToken   [:test/user-token nil]
    :UserTokenIn [:test/user-token true]
    ;;
    :UserValues   [:test/user-values nil]
    :UserValuesIn [:test/user-values true]))

;;; --------------------------------------------------------------------------------

(deftest refs->fields
  (is (= (-> :scalar/string lc/canonicalise-ref lc/ref->field)
         {:type        :String
          :description "Non Blank String"}))

  (is (= (lc/ref->field {:type        :scalar/string
                         :description "Something Else"})
         {:type        :String
          :description "Something Else"}))

  (is (= (-> :scalar/string-2 lc/canonicalise-ref lc/ref->field)
         {:type        :StrangeString
          :description "Like a String but stranger."}))

  (is (= (-> :test/user-id lc/canonicalise-ref lc/ref->field)
         {:type :String}))

  (is (= (lc/ref->field {:type        :test/user-id
                         :description "User ID"})
         {:type        :String
          :description "User ID"}))

  (is (= (lc/ref->field {:type :test/user-id}
                        :required? true)
         {:type        '(non-null :String)}))
  ;;
  ;; Handling lists
  (is (= (-> {:type [:scalar/string]}
             lc/canonicalise-ref
             lc/ref->field)
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
    (is (= (lc/enum->enum {:type        :test
                           :values      [:this :is :values]
                           :description "What evs"})
           [:Test {:values      [:THIS :IS :VALUES]
                   :description "What evs"}])))

  (testing "SP Transforms"
    (is (= (-> :scalar/au-state-1 lc/canonicalise-enum lc/enum-ref->enum)
           [:AuState1
            {:values [:ACT :NSW :NT :QLD :SA :TAS :VIC :WA],
             :description "An Australian State or Territory"}]))

    (is (= (-> :scalar/au-state-2 lc/canonicalise-enum lc/enum-ref->enum)
           [:AuState2
            {:values [:ACT :NSW :NT :QLD :SA :TAS :VIC :WA],
             :description "An Australian State or Territory for GQL."}]))

    (is (= (-> :scalar/au-state-3 lc/canonicalise-enum lc/enum-ref->enum)
           [:AusState
            {:values [:ACT :NSW :NT :QLD :SA :TAS :VIC :WA],
             :description "An Australian State or Territory for GQL, with a different key."}]))

    (is (= (-> :test/user-role-type lc/canonicalise-enum lc/enum-ref->enum)
           [:UserRoleType {:values [:ONE :TWO :THREE]}])))

  (testing "Can't Transform"
    (is (thrown? clojure.lang.ExceptionInfo
                 (lc/enum->enum :invalid)))))

(deftest entity-refs->objects
  (is (= (-> :test/user
             lc/canonicalise-ref
             lc/entity-ref->object)
         [:User {:fields {:userId     {:type :String}
                          :givenName  {:type :String}
                          :familyName {:type :String}
                          :dob        {:type :JuDate}
                          :height     {:type :Integer}
                          :isCitizen  {:type :Boolean}}}]))

  (is (= (-> :test/user-token
             lc/canonicalise-ref
             lc/entity-ref->object)
         [:UserToken {:fields {:userId {:type :String}}}]))

  (is (= (-> :test/user-values
             lc/canonicalise-ref
             lc/entity-ref->object)
         [:UserValues {:fields {:givenName  {:type :String}
                                :familyName {:type :String}
                                :dob        {:type :JuDate}
                                :height     {:type :Integer}
                                :isCitizen  {:type :Boolean}}}]))

  (is (= (-> :test/user-values
             lc/canonicalise-ref
             (lc/entity-ref->object :in? true))
         '[:UserValuesIn {:fields {:givenName  {:type :String}
                                   :familyName {:type (non-null :String)}
                                   :dob        {:type :JuDate}
                                   :height     {:type :Integer}
                                   :isCitizen  {:type :Boolean}}}]))

  (is (= (-> :test/user-role
             lc/canonicalise-ref
             lc/entity-ref->object)
         [:UserRole {:fields      {:userId       {:type :String}
                                   :userRoleAttr {:type :UserRoleType}}
                     :description "Links a user to a Role they can perform."}]))

  (is (= (-> :test/user-role
             lc/canonicalise-ref
             (lc/entity-ref->object :in? true))
         '[:UserRoleIn
           {:fields {:userId       {:type (non-null :String)}
                     :userRoleAttr {:type (non-null :UserRoleType)}}
            :description "Links a user to a Role they can perform."}])))

(deftest objects->objects
  (is (= (lc/object->object {:type        :object-1
                             :description "Blah"
                             :fields      {:pattern :string
                                           :user    :test/user
                                           :user-2  {:type      :test/user
                                                     :required? true}}})
         '[:Object1 {:fields {:pattern {:type :String}
                              :user    {:type :User}
                              :user2   {:type (non-null :User)}}
                     :description "Blah"}])))

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
  (is (= (lc/referenced-entity-types :test/parent-entity #{})
         #{:test/grand-child-entity :test/child-entity}))

  (is (= (lc/referenced-enum-types :test/grand-child-entity)
         #{:test/enum-type-1 :test/enum-type-2})))

(def +q1+ {:fetch-user       {:type     :test/user
                              :args     {:token {:type      :test/user-token
                                                 :required? true}}
                              :resolver 'fetch-user}
           :fetch-user-by-id {:type     :test/user
                              :args     {:user-id :string}
                              :resolver 'fetch-user}
           :fetch-users      {:type     [:test/user]
                              :resolver 'fetch-user}
           ;;
           :fetch-user-roles    {:type     [:test/user-role]
                                 :args     {:token {:type      :test/user-token
                                                    :required? true}}
                                 :resolver 'fetch-user-roles}
           :fetch-users-by-role {:type     [:test/user]
                                 :args     {:role-type {:type      :test/user-role-type
                                                        :required? true}}
                                 :resolver 'fetch-user-roles}})

(def +m1+ {:add-user    {:type :test/user
                         :args {:record {:type      :test/user-values
                                         :required? true}}}
           :modify-user {:type :test/user
                         :args {:record {:type      :test/user
                                         :required? true}}}
           :remove-user {:type :boolean
                         :args {:record {:type      :test/user-token
                                         :required? true}}}})

(deftest endpoint-refs
  (is (= (lc/endpoint-types->refs (merge +q1+ +m1+))
         [{:type :test/user}
          {:type :test/user-role :many? true}]))

  (is (= (lc/endpoint-args->refs (merge +q1+ +m1+))
         [{:type :test/user           :required? true}
          {:type :test/user-role-type :required? true}
          {:type :test/user-token     :required? true}
          {:type :test/user-values    :required? true}])))

(deftest generate-schema-1
  (let [{:keys [enums objects input-objects
                queries mutations]}
        (lc/generate-schema {:enums     {:enum-1 {:values      [:one :two :three]
                                                  :description "Enum 1"}}
                             :objects  {:object-1 {:description "Blah"
                                                   :fields      {:pattern :string
                                                                 :user    :test/user
                                                                 :user-2  {:type      :test/user
                                                                           :required? true}}}}
                             :queries   +q1+
                             :mutations +m1+})]
    (is (= enums
           {:Enum1        {:values [:ONE :TWO :THREE] :description "Enum 1"}
            :UserRoleType {:values [:ONE :TWO :THREE]}}))
    (is (= objects
           '{:Object1 {:fields {:pattern {:type :String}
                                :user    {:type :User}
                                :user2   {:type (non-null :User)}}
                       :description "Blah"}
             :User    {:fields
                       {:userId     {:type :String}
                        :givenName  {:type :String}
                        :familyName {:type :String}
                        :dob        {:type :JuDate}
                        :height     {:type :Integer}
                        :isCitizen  {:type :Boolean}}}
             :UserRole {:fields {:userId       {:type :String}
                                 :userRoleAttr {:type :UserRoleType}}
                        :description "Links a user to a Role they can perform."}}))
    (is (= input-objects
           '{:UserIn       {:fields {:userId     {:type (non-null :String)}
                                     :givenName  {:type :String}
                                     :familyName {:type (non-null :String)}
                                     :dob        {:type :JuDate}
                                     :height     {:type :Integer}
                                     :isCitizen  {:type :Boolean}}}
             :UserTokenIn  {:fields {:userId {:type (non-null :String)}}}
             :UserValuesIn {:fields {:givenName  {:type :String}
                                     :familyName {:type (non-null :String)}
                                     :dob        {:type :JuDate}
                                     :height     {:type :Integer}
                                     :isCitizen  {:type :Boolean}}}}))
    (is (= queries
           '{:fetchUser        {:type     :User
                                :args     {:token {:type (non-null :UserToken)}}
                                :resolver fetch-user}
             :fetchUserById    {:type     :User
                                :args     {:userId {:type :String}}
                                :resolver fetch-user}
             :fetchUsers       {:type     (list (non-null :User))
                                :resolver fetch-user}
             :fetchUserRoles   {:type     (list (non-null :UserRole))
                                :args     {:token {:type (non-null :UserToken)}}
                                :resolver fetch-user-roles}
             :fetchUsersByRole {:type     (list (non-null :User))
                                :args     {:roleType {:type :UserRoleType}}
                                :resolver fetch-user-roles}}))
    (is (= mutations
           '{:addUser    {:type :UserIn
                          :args {:record {:type (non-null :UserValues)}}}
             :modifyUser {:type :UserIn
                          :args {:record {:type (non-null :User)}}}
             :removeUser {:type :Boolean
                          :args {:record {:type (non-null :UserToken)}}}}))))
