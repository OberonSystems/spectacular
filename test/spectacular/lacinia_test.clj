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
(sp/clear! :s :a :e)

(defn java-util-date?
  [d]
  (instance? java.util.Date d))

;; Some common scalars that we'll use in subsequent tests
(sp/scalar :s/string        string? ::sp/description "Non Blank String")
(sp/scalar :s/string-2      string?
           ::sp/description "Non Blank String"
           ;;
           ::lc/type        :strange-string
           ::lc/description "Like a String but stranger.")
(sp/scalar :s/boolean boolean?)
(sp/scalar :s/integer integer?
           ::lc/type :int)
(sp/scalar :s/ju-date java-util-date? ::sp/description "Java Date")

;;; --------------------------------------------------------------------------------

(sp/attribute :a/user-id       :s/string)
(sp/attribute :a/given-name    :s/string)
(sp/attribute :a/family-name   :s/string)
(sp/attribute :a/dob           :s/ju-date)
(sp/attribute :a/height        :s/integer)
(sp/attribute :a/qualification :s/string-2)
(sp/attribute :a/citizen?      :s/boolean
              ::lc/name :is-citizen)

(sp/entity :e/user
           [:a/user-id
            :a/given-name
            :a/family-name
            :a/dob
            :a/height
            :a/qualification
            :a/citizen?]
           ::sp/identity-keys [:a/user-id]
           ::sp/required-keys [:a/family-name])

(sp/enum      :s/user-role [:one :two :three])
(sp/attribute :a/user-role :s/user-role)

(sp/entity :e/user-role
           [:a/user-id
            :a/user-role]
           ::sp/identity-keys [:a/user-id :a/user-role]
           ;;
           ::lc/description "Links a user to a Role they can perform.")

(sp/entity-token  :e/user-token  :e/user)
(sp/entity-values :e/user-values :e/user)

;;; --------------------------------------------------------------------------------

(deftest refs->field-types
  ;; Add some tests so that only entities get the In suffix.
  ;;
  ;; Also ensure one of the entities has a different :lc/type and
  ;; check that.
  (are [lhs rhs] (= lhs (let [ref (-> rhs first lc/canonicalise-ref)
                              in? (-> rhs second)]
                          (lc/ref->field-type ref :in? in?)))
    :StrangeString [:s/string-2 nil]
    :StrangeString [:s/string-2 true]

    :UserRole   [:e/user-role nil]
    :UserRoleIn [:e/user-role true]
    ;;
    :User   [:e/user nil]
    :UserIn [:e/user true]
    ;;
    :UserToken   [:e/user-token nil]
    :UserTokenIn [:e/user-token true]
    ;;
    :UserValues   [:e/user-values nil]
    :UserValuesIn [:e/user-values true]))

;;; --------------------------------------------------------------------------------

(deftest refs->fields
  (is (= (-> :s/string lc/canonicalise-ref lc/ref->field)
         {:type        :String
          :description "Non Blank String"}))

  (is (= (lc/ref->field {:type        :s/string
                         :description "Something Else"})
         {:type        :String
          :description "Something Else"}))

  (is (= (-> :s/string-2 lc/canonicalise-ref lc/ref->field)
         {:type        :StrangeString
          :description "Like a String but stranger."}))

  (is (= (-> :a/user-id lc/canonicalise-ref lc/ref->field)
         {:type :String}))

  (is (= (lc/ref->field {:type        :a/user-id
                         :description "User ID"})
         {:type        :String
          :description "User ID"}))

  (is (= (lc/ref->field {:type :a/user-id}
                        :required? true)
         {:type        '(non-null :String)}))
  ;;
  ;; Handling lists
  (is (= (-> {:type [:s/string]}
             lc/canonicalise-ref
             lc/ref->field)
         {:type        '(list (non-null :String))
          :description "Non Blank String"})))

;;; --------------------------------------------------------------------------------

(sp/enum   :s/au-state-1 [:act :nsw :nt :qld :sa :tas :vic :wa]
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

(sp/enum :s/au-state-2 [:act :nsw :nt :qld :sa :tas :vic :wa]
         ::sp/description "An Australian State or Territory"
         ::lc/description "An Australian State or Territory for GQL.")

(sp/enum :s/au-state-3 [:act :nsw :nt :qld :sa :tas :vic :wa]
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
    (is (= (-> :s/au-state-1 lc/canonicalise-enum lc/enum-ref->enum)
           [:AuState1
            {:values [:ACT :NSW :NT :QLD :SA :TAS :VIC :WA],
             :description "An Australian State or Territory"}]))

    (is (= (-> :s/au-state-2 lc/canonicalise-enum lc/enum-ref->enum)
           [:AuState2
            {:values [:ACT :NSW :NT :QLD :SA :TAS :VIC :WA],
             :description "An Australian State or Territory for GQL."}]))

    (is (= (-> :s/au-state-3 lc/canonicalise-enum lc/enum-ref->enum)
           [:AusState
            {:values [:ACT :NSW :NT :QLD :SA :TAS :VIC :WA],
             :description "An Australian State or Territory for GQL, with a different key."}]))

    (is (= (-> :s/user-role lc/canonicalise-enum lc/enum-ref->enum)
           [:UserRole {:values [:ONE :TWO :THREE]}])))

  (testing "Can't Transform"
    (is (thrown? clojure.lang.ExceptionInfo
                 (lc/enum->enum :invalid)))))

(deftest entity-refs->objects
  (is (= (-> :e/user
             lc/canonicalise-ref
             lc/entity-ref->object)
         [:User {:fields {:userId        {:type :String}
                          :givenName     {:type :String}
                          :familyName    {:type :String}
                          :dob           {:type :JuDate}
                          :height        {:type :Int}
                          :qualification {:type :StrangeString}
                          :isCitizen     {:type :Boolean}}}]))

  (is (= (-> :e/user-token
             lc/canonicalise-ref
             lc/entity-ref->object)
         [:UserToken {:fields {:userId {:type :String}}}]))

  (is (= (-> :e/user-values
             lc/canonicalise-ref
             lc/entity-ref->object)
         [:UserValues {:fields {:givenName     {:type :String}
                                :familyName    {:type :String}
                                :dob           {:type :JuDate}
                                :height        {:type :Int}
                                :qualification {:type :StrangeString}
                                :isCitizen     {:type :Boolean}}}]))

  (is (= (-> :e/user-values
             lc/canonicalise-ref
             (lc/entity-ref->object :in? true))
         '[:UserValuesIn {:fields {:givenName     {:type :String}
                                   :familyName    {:type (non-null :String)}
                                   :dob           {:type :JuDate}
                                   :height        {:type :Int}
                                   :qualification {:type :StrangeString}
                                   :isCitizen     {:type :Boolean}}}]))

  (is (= (-> :e/user-role
             lc/canonicalise-ref
             lc/entity-ref->object)
         '[:UserRole {:fields {:userId   {:type :String}
                               :userRole {:type :UserRole}}
                      :description "Links a user to a Role they can perform."}]))

  (is (= (-> :e/user-role
             lc/canonicalise-ref
             (lc/entity-ref->object :in? true))
         '[:UserRoleIn {:fields {:userId   {:type (non-null :String)}
                                 :userRole {:type (non-null :UserRole)}}
                        :description "Links a user to a Role they can perform."}])))

(deftest objects->objects
  (is (= (lc/object->object {:type        :object-1
                             :description "Blah"
                             :fields      {:pattern :string
                                           :user    :e/user
                                           :user-2  {:type      :e/user
                                                     :required? true}}})
         '[:Object1 {:fields {:pattern {:type :String}
                              :user    {:type :User}
                              :user2   {:type (non-null :User)}}
                     :description "Blah"}])))

(sp/enum :s/enum-type-1 [:one  :two  :three])
(sp/enum :s/enum-type-2 [:four :five :six])

(sp/attribute :a/enum-1 :s/enum-type-1)
(sp/attribute :a/enum-2 :s/enum-type-2)
(sp/entity    :e/grand-child [:a/enum-1 :a/enum-2])

(sp/attribute :a/grand-child :e/grand-child)
(sp/entity    :e/child       [:a/grand-child])

(sp/attribute :a/child  :e/child)
(sp/entity    :e/parent [:a/child])

(deftest referenced-entities
  (is (= (lc/referenced-entity-types :e/parent #{})
         #{:e/grand-child :e/child}))

  (is (= (lc/referenced-enum-types :e/grand-child)
         #{:s/enum-type-1 :s/enum-type-2})))

(def +q1+ {:fetch-user       {:type     :e/user
                              :args     {:token {:type      :e/user-token
                                                 :required? true}}
                              :resolve  'fetch-user}
           :fetch-user-by-id {:type     :e/user
                              :args     {:user-id :string}
                              :resolve  'fetch-user}
           :fetch-users      {:type     [:e/user]
                              :resolve  'fetch-user}
           ;;
           :fetch-user-roles    {:type     [:e/user-role]
                                 :args     {:token {:type      :e/user-token
                                                    :required? true}}
                                 :resolve  'fetch-user-roles}
           :fetch-users-by-role {:type     [:e/user]
                                 :args     {:role-type {:type      :s/user-role
                                                        :required? true}}
                                 :resolve  'fetch-user-roles}})

(def +m1+ {:add-user    {:type :e/user
                         :args {:record {:type      :e/user-values
                                         :required? true}}}
           :modify-user {:type :e/user
                         :args {:record {:type      :e/user
                                         :required? true}}}
           :remove-user {:type :boolean
                         :args {:record {:type      :e/user-token
                                         :required? true}}}})

(deftest endpoint-refs
  (is (= (lc/endpoint-types->refs (merge +q1+ +m1+))
         [{:type :e/user}
          {:type :e/user-role :many? true}]))

  (is (= (lc/endpoint-args->refs (merge +q1+ +m1+))
         [{:type :e/user        :required? true}
          {:type :e/user-token  :required? true}
          {:type :e/user-values :required? true}
          {:type :s/user-role   :required? true}])))

(deftest generate-schema-1
  (let [{:keys [enums objects input-objects
                queries mutations]}
        (lc/generate-schema {:enums    {:enum-1 {:values      [:one :two :three]
                                                 :description "Enum 1"}}
                             :objects  {:object-1 {:description "Blah"
                                                   :fields      {:pattern :string
                                                                 :user    :e/user
                                                                 :user-2  {:type      :e/user
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
                       {:userId        {:type :String}
                        :givenName     {:type :String}
                        :familyName    {:type :String}
                        :dob           {:type :JuDate}
                        :height        {:type :Int}
                        :qualification {:type :StrangeString}
                        :isCitizen     {:type :Boolean}}}
             :UserRole {:fields {:userId   {:type :String}
                                 :userRole {:type :UserRole}}
                        :description "Links a user to a Role they can perform."}}))
    (is (= input-objects
           '{:UserIn       {:fields {:userId        {:type (non-null :String)}
                                     :givenName     {:type :String}
                                     :familyName    {:type (non-null :String)}
                                     :dob           {:type :JuDate}
                                     :height        {:type :Int}
                                     :qualification {:type :StrangeString}
                                     :isCitizen     {:type :Boolean}}}
             :UserTokenIn  {:fields {:userId {:type (non-null :String)}}}
             :UserValuesIn {:fields {:givenName     {:type :String}
                                     :familyName    {:type (non-null :String)}
                                     :dob           {:type :JuDate}
                                     :height        {:type :Int}
                                     :qualification {:type :StrangeString}
                                     :isCitizen     {:type :Boolean}}}}))
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
             :fetchUsersByRole {:type    (list (non-null :User))
                                :args    {:roleType {:type :UserRole}}
                                :resolve fetch-user-roles}}))
    (is (= mutations
           '{:addUser    {:type :User
                          :args {:record {:type (non-null :UserValuesIn)}}}
             :modifyUser {:type :User
                          :args {:record {:type (non-null :UserIn)}}}
             :removeUser {:type :Boolean
                          :args {:record {:type (non-null :UserTokenIn)}}}}))))

(deftest generate-schema-with-edges
  ;; Testing that :objects with resolvers get merged in correctly.
  (is (= (lc/generate-schema {:edges   {:e/user {:user-roles {:type    [:e/user-role]
                                                              :resolve 'users-user-roles}}}
                              :queries {:fetch-user {:type    [:e/user]
                                                     :resolve 'fetch-users}}})
         '{:objects {:User {:fields
                            {:userId        {:type :String}
                             :givenName     {:type :String}
                             :familyName    {:type :String}
                             :dob           {:type :JuDate}
                             :height        {:type :Int}
                             :qualification {:type :StrangeString}
                             :isCitizen     {:type :Boolean}
                             :userRoles     {:type        (list (non-null :UserRole))
                                             :description "Links a user to a Role they can perform."
                                             :resolve     users-user-roles}}}}
           :queries {:fetchUser {:type    (list (non-null :User))
                                 :resolve fetch-users}}})))
