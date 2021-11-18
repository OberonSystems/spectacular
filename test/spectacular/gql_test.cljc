(ns spectacular.core-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]])
  (:require [spectacular.core :as sp]
            [spectacular.graphql :as gql]
            :reload))

;; Some common scalars that we'll use in subsequent tests
(sp/scalar :scalar/string  string? ::sp/description "Non Blank String")
(sp/scalar :scalar/boolean boolean?)
(sp/scalar :scalar/integer integer?)

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
         ::gql/description "An Australian State or Territory for GQL.")

(sp/enum :scalar/au-state-3 [:act :nsw :nt :qld :sa :tas :vic :wa]
         ::sp/description "An Australian State or Territory"
         ::gql/type        :aus-state
         ::gql/description "An Australian State or Territory for GQL.")

(deftest transform-enums
  (testing "Inline Transforms"
    (is (= (gql/transform-enum {:key :test :values [:this :is :values] :description "What evs"})
           [:Test {:values [:THIS :IS :VALUES], :description "What evs"}])))

  (testing "SP Transforms"
    (is (= (gql/transform-enum :scalar/au-state-1)
           [:AuState1
            {:values [:ACT :NSW :NT :QLD :SA :TAS :VIC :WA],
             :description "An Australian State or Territory"}]))

    (is (= (gql/transform-enum :scalar/au-state-2)
           [:AuState2
            {:values [:ACT :NSW :NT :QLD :SA :TAS :VIC :WA],
             :description "An Australian State or Territory for GQL."}]))

    (is (= (gql/transform-enum :scalar/au-state-3)
           [:AusState
            {:values [:ACT :NSW :NT :QLD :SA :TAS :VIC :WA],
             :description "An Australian State or Territory for GQL."}])))

  (testing "Can't Transform"
    (is (thrown? clojure.lang.ExceptionInfo
                 (gql/transform-enum :asdf)))))

;;; --------------------------------------------------------------------------------

(sp/attribute :ab/street :scalar/string     ::sp/label "Street")
(sp/attribute :ab/state  :scalar/au-state-1 ::sp/label "State")

(sp/scalar :scalar/date #(instance? java.util.Date %) ::sp/description "Java Date")
(sp/attribute :ab/day    :scalar/date
              ::sp/label "Day"
              ::gql/type :string)

(sp/attribute :ab/status :scalar/integer
              ;;
              ::sp/label "Computed Field"
              ;;
              ::gql/description "This is a resolver"
              ::gql/resolver    'dummy-function)

(deftest transform-attributes
  (= (gql/transform-attr :ab/street)
     [:Street {:type 'String}])

  (= (gql/transform-attr {:key :ab/street :description "Street"})
     [:Street {:type 'String :description "Street"}])

  (= (gql/transform-attr :ab/status)
     [:Status {:type        'Integer
               :description "This is a resolver"
               :resolver    'dummy-function}]))

;;; --------------------------------------------------------------------------------

(sp/entity :ab/address
           [:ab/street
            :ab/state]
           ::sp/required-keys [:ab/state]
           ::sp/label         "Address"
           ::sp/description   "An Australian Address")

(deftest transform-entities-1
  (= (gql/transform-query-object :ab/address)
     [:Address {:fields {:Street {:type 'String}
                         :State  {:type 'AuState1}}
                :description "An Australian Address"}])

  (= (gql/transform-input-object :ab/address)
     [:Address {:fields {:Street {:type 'String}
                         :State  {:type '(non-null AuState1)}}
                :description "An Australian Address"}]))
