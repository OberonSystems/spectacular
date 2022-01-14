(ns spectacular.lacinia-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]])
  (:require [spectacular.core    :as sp]
            [spectacular.lacinia :as lc]
            :reload))

;; Ensure the stuff we want to add to the registtry is cleared out on
;; recompile.
(sp/clear! :scalar :attr :entity)

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

(sp/attribute :attr/user-id     :scalar/string)
(sp/attribute :attr/given-name  :scalar/string)
(sp/attribute :attr/family-name :scalar/string)
(sp/attribute :attr/dob         :scalar/ju-date)
(sp/attribute :attr/height      :scalar/integer)
(sp/attribute :attr/citizen?    :scalar/boolean
              ::lc/name :is-citizen)

(sp/entity :entity/user
           [:attr/user-id
            :attr/given-name
            :attr/family-name
            :attr/dob
            :attr/height
            :attr/citizen?]
           ::sp/identity-keys [:attr/user-id]
           ::sp/required-keys [:attr/family-name])

(sp/enum      :scalar/user-role [:one :two :three])
(sp/attribute :attr/user-role :scalar/user-role)

(sp/entity :entity/user-role
           [:attr/user-id
            :attr/user-role]
           ::sp/identity-keys [:attr/user-id :attr/user-role]
           ;;
           ::lc/description "Links a user to a Role they can perform.")

(sp/entity-token  :entity/user-token  :entity/user)
(sp/entity-values :entity/user-values :entity/user)

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

    :UserRole   [:entity/user-role nil]
    :UserRoleIn [:entity/user-role true]
    ;;
    :User   [:entity/user nil]
    :UserIn [:entity/user true]
    ;;
    :UserToken   [:entity/user-token nil]
    :UserTokenIn [:entity/user-token true]
    ;;
    :UserValues   [:entity/user-values nil]
    :UserValuesIn [:entity/user-values true]))

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

  (is (= (-> :attr/user-id lc/canonicalise-ref lc/ref->field)
         {:type :String}))

  (is (= (lc/ref->field {:type        :attr/user-id
                         :description "User ID"})
         {:type        :String
          :description "User ID"}))

  (is (= (lc/ref->field {:type :attr/user-id}
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

    (is (= (-> :scalar/user-role lc/canonicalise-enum lc/enum-ref->enum)
           [:UserRole {:values [:ONE :TWO :THREE]}])))

  (testing "Can't Transform"
    (is (thrown? clojure.lang.ExceptionInfo
                 (lc/enum->enum :invalid)))))

(deftest entity-refs->objects
  (is (= (-> :entity/user
             lc/canonicalise-ref
             lc/entity-ref->object)
         [:User {:fields {:userId     {:type :String}
                          :givenName  {:type :String}
                          :familyName {:type :String}
                          :dob        {:type :JuDate}
                          :height     {:type :Integer}
                          :isCitizen  {:type :Boolean}}}]))

  (is (= (-> :entity/user-token
             lc/canonicalise-ref
             lc/entity-ref->object)
         [:UserToken {:fields {:userId {:type :String}}}]))

  (is (= (-> :entity/user-values
             lc/canonicalise-ref
             lc/entity-ref->object)
         [:UserValues {:fields {:givenName  {:type :String}
                                :familyName {:type :String}
                                :dob        {:type :JuDate}
                                :height     {:type :Integer}
                                :isCitizen  {:type :Boolean}}}]))

  (is (= (-> :entity/user-values
             lc/canonicalise-ref
             (lc/entity-ref->object :in? true))
         '[:UserValuesIn {:fields {:givenName  {:type :String}
                                   :familyName {:type (non-null :String)}
                                   :dob        {:type :JuDate}
                                   :height     {:type :Integer}
                                   :isCitizen  {:type :Boolean}}}]))

  (is (= (-> :entity/user-role
             lc/canonicalise-ref
             lc/entity-ref->object)
         '[:UserRole {:fields {:userId   {:type :String}
                               :userRole {:type :UserRole}}
                      :description "Links a user to a Role they can perform."}]))

  (is (= (-> :entity/user-role
             lc/canonicalise-ref
             (lc/entity-ref->object :in? true))
         '[:UserRoleIn {:fields {:userId   {:type (non-null :String)}
                                 :userRole {:type (non-null :UserRole)}}
                        :description "Links a user to a Role they can perform."}])))

(deftest objects->objects
  (is (= (lc/object->object {:type        :object-1
                             :description "Blah"
                             :fields      {:pattern :string
                                           :user    :entity/user
                                           :user-2  {:type      :entity/user
                                                     :required? true}}})
         '[:Object1 {:fields {:pattern {:type :String}
                              :user    {:type :User}
                              :user2   {:type (non-null :User)}}
                     :description "Blah"}])))

(sp/enum :scalar/enum-type-1 [:one  :two  :three])
(sp/enum :scalar/enum-type-2 [:four :five :six])

(sp/attribute :attr/enum-1 :scalar/enum-type-1)
(sp/attribute :attr/enum-2 :scalar/enum-type-2)
(sp/entity    :entity/grand-child [:attr/enum-1 :attr/enum-2])

(sp/attribute :attr/grand-child :entity/grand-child)
(sp/entity    :entity/child      [:attr/grand-child])

(sp/attribute :attr/child  :entity/child)
(sp/entity    :entity/parent [:attr/child])

(deftest referenced-entities
  (is (= (lc/referenced-entity-types :entity/parent #{})
         #{:entity/grand-child :entity/child}))

  (is (= (lc/referenced-enum-types :entity/grand-child)
         #{:scalar/enum-type-1 :scalar/enum-type-2})))

(def +q1+ {:fetch-user       {:type     :entity/user
                              :args     {:token {:type      :entity/user-token
                                                 :required? true}}
                              :resolve  'fetch-user}
           :fetch-user-by-id {:type     :entity/user
                              :args     {:user-id :string}
                              :resolve  'fetch-user}
           :fetch-users      {:type     [:entity/user]
                              :resolve  'fetch-user}
           ;;
           :fetch-user-roles    {:type     [:entity/user-role]
                                 :args     {:token {:type      :entity/user-token
                                                    :required? true}}
                                 :resolve  'fetch-user-roles}
           :fetch-users-by-role {:type     [:entity/user]
                                 :args     {:role-type {:type      :scalar/user-role
                                                        :required? true}}
                                 :resolve  'fetch-user-roles}})

(def +m1+ {:add-user    {:type :entity/user
                         :args {:record {:type      :entity/user-values
                                         :required? true}}}
           :modify-user {:type :entity/user
                         :args {:record {:type      :entity/user
                                         :required? true}}}
           :remove-user {:type :boolean
                         :args {:record {:type      :entity/user-token
                                         :required? true}}}})

(deftest endpoint-refs
  (is (= (lc/endpoint-types->refs (merge +q1+ +m1+))
         [{:type :entity/user}
          {:type :entity/user-role :many? true}]))

  (is (= (lc/endpoint-args->refs (merge +q1+ +m1+))
         [{:type :entity/user        :required? true}
          {:type :entity/user-token  :required? true}
          {:type :entity/user-values :required? true}
          {:type :scalar/user-role   :required? true}])))

(deftest generate-schema-1
  (let [{:keys [enums objects input-objects
                queries mutations]}
        (lc/generate-schema {:enums     {:enum-1 {:values      [:one :two :three]
                                                  :description "Enum 1"}}
                             :objects  {:object-1 {:description "Blah"
                                                   :fields      {:pattern :string
                                                                 :user    :entity/user
                                                                 :user-2  {:type      :entity/user
                                                                           :required? true}}}}
                             :queries   +q1+
                             :mutations +m1+})]
    (is (= enums
           {:Enum1    {:values [:ONE :TWO :THREE] :description "Enum 1"}
            :UserRole {:values [:ONE :TWO :THREE]}}))
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
             :UserRole {:fields {:userId   {:type :String}
                                 :userRole {:type :UserRole}}
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
           '{:fetchUser        {:type    :User
                                :args    {:token {:type (non-null :UserTokenIn)}}
                                :resolve fetch-user}
             :fetchUserById    {:type    :User
                                :args    {:userId {:type :String}}
                                :resolve fetch-user}
             :fetchUsers       {:type    (list (non-null :User))
                                :resolve fetch-user}
             :fetchUserRoles   {:type    (list (non-null :UserRole))
                                :args    {:token {:type (non-null :UserTokenIn)}}
                                :resolve fetch-user-roles}
             :fetchUsersByRole {:type (list (non-null :User))
                                :args {:roleType {:type :UserRole}}
                                :resolve fetch-user-roles}}))
    (is (= mutations
           '{:addUser    {:type :User
                          :args {:record {:type (non-null :UserValuesIn)}}}
             :modifyUser {:type :User
                          :args {:record {:type (non-null :UserIn)}}}
             :removeUser {:type :Boolean
                          :args {:record {:type (non-null :UserTokenIn)}}}}))))
