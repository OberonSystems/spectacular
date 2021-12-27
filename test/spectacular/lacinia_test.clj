(ns spectacular.lacinia-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.data :refer [diff]])
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
(sp/scalar :scalar/ju-date java-util-date?)

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

(deftest enums->schema
  (testing "Inline Transforms"
    (is (= (lc/enum->schema {:key :test :values [:this :is :values] :description "What evs"})
           [:Test
            {:values [:THIS :IS :VALUES], :description "What evs"}])))

  (testing "SP Transforms"
    (is (= (lc/enum->schema :scalar/au-state-1)
           [:AuState1
            {:values [:ACT :NSW :NT :QLD :SA :TAS :VIC :WA],
             :description "An Australian State or Territory"}]))

    (is (= (lc/enum->schema :scalar/au-state-2)
           [:AuState2
            {:values [:ACT :NSW :NT :QLD :SA :TAS :VIC :WA],
             :description "An Australian State or Territory for GQL."}]))

    (is (= (lc/enum->schema :scalar/au-state-3)
           [:AusState
            {:values [:ACT :NSW :NT :QLD :SA :TAS :VIC :WA],
             :description "An Australian State or Territory for GQL, with a different key."}])))

  (testing "Can't Transform"
    (is (thrown? clojure.lang.ExceptionInfo
                 (lc/enum->schema :asdf)))))

;;; --------------------------------------------------------------------------------

(sp/attribute :ab/street :scalar/string     ::sp/label "Street")
(sp/attribute :ab/state  :scalar/au-state-1 ::sp/label "State")

(sp/scalar :scalar/date #(instance? java.util.Date %) ::sp/description "Java Date")
(sp/attribute :ab/day    :scalar/date
              ::sp/label "Day"
              ::lc/type :string)

(sp/attribute :ab/status :scalar/integer
              ;;
              ::sp/label "Computed Field"
              ;;
              ::lc/description "This has a resolver"
              ::lc/resolver    'dummy-function)

(deftest transform-fields
  (is (= (lc/field->schema :scalar/string)
         {:type        :String
          :description "Non Blank String"}))

  (is (= (lc/field->schema :scalar/string)
         {:type        :String
          :description "Non Blank String"}))

  (is (= (lc/field->schema {:type        :scalar/string
                            :description "Something Else"})
         {:type        :String
          :description "Something Else"}))

  (is (= (lc/field->schema {:type        :scalar/string
                            :description "Something Else"
                            ;;
                            ::lc/type   :strange-string})
         {:type        :StrangeString
          :description "Something Else"}))

  (is (= (lc/field->schema :ab/street)
         {:type :String}))

  (is (= (lc/field->schema {:type        :ab/street
                            :description "Special Street"})
         {:type        :String
          :description "Special Street"}))

  (is (= (lc/field->schema {:type        :ab/street
                            :required?   true
                            :description "Special Street"})
         {:type        '(non-null :String)
          :description "Special Street"})))

;;; --------------------------------------------------------------------------------

(sp/entity :ab/address
           [:ab/street
            :ab/state]
           ::sp/required-keys [:ab/state]
           ::sp/label         "Address"
           ::sp/description   "An Australian Address")

(deftest transform-entities-1
  (= (lc/transform-query-object :ab/address)
     [:Address {:fields {:Street {:type 'String}
                         :State  {:type 'AuState1}}
                :description "An Australian Address"}])

  (= (lc/transform-input-object :ab/address)
     [:Address {:fields {:Street {:type 'String}
                         :State  {:type '(non-null AuState1)}}
                :description "An Australian Address"}]))

;;; --------------------------------------------------------------------------------

;; Make this a more complex validator later, also have a different GQL
;; type for it.
(sp/scalar :scalar/e164 string?
           ::lc/type :string)

(sp/attribute :ab/contact-id  :scalar/integer)
(sp/attribute :ab/given-name  :scalar/string)
(sp/attribute :ab/family-name :scalar/string)
(sp/attribute :ab/phone-no    :scalar/e164)

(sp/attribute :ab/address-id  :scalar/integer)

(sp/entity :ab/contact
           [:ab/contact-id
            :ab/given-name
            :ab/family-name
            :ab/phone-no]
           ::sp/identity-keys [:ab/contact-id]
           ::sp/required-keys [:ab/contact-id
                               :ab/given-name
                               :ab/phone-no])

(sp/entity :ab/contact-address
           [:ab/contact-id
            :ab/address-id]
           ::sp/identity-keys [:ab/contact-id :ab/address-id]
           ::sp/required-keys [:ab/contact-id :ab/address-id])

(deftest transform-query-arg

  (is (= (lc/transform-query-arg :verbose? {:type :boolean})
         {:isVerbose {:type 'Boolean}}))

  (is (= (lc/transform-query-arg :verbose? {:type      :boolean
                                             :required? true})
         {:isVerbose {:type '(non-null Boolean)}}))

  (is (= (lc/transform-query-arg :verbose? {:type      :boolean
                                             :required? true
                                             ;;
                                             ::lc/key  :hasVerbosity})
         {:hasVerbosity {:type '(non-null Boolean)}}))

;;;

  (is (= (lc/transform-query-arg :token {:type   :ab/contact
                                          :token? true})
         {:token {:type 'ContactToken}}))

  (is (= (lc/transform-query-arg :token {:type      :ab/contact
                                          :token?    true
                                          :required? true})
         {:token {:type '(non-null ContactToken)}}))

  (is (= (lc/transform-query-arg :record {:type :ab/contact})
         {:record {:type 'Contact}})))

;;; --------------------------------------------------------------------------------

#_(
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
            ::sp/identity-keys [:test/user-id :test/user-role]))

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