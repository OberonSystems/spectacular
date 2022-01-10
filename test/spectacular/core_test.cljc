(ns spectacular.core-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]])
  (:require [spectacular.core :as sp]
            :reload))

;;; --------------------------------------------------------------------------------
;;  Lets do an address book example
;;

;; Assuming :ex is an :example namespace
;;
;; :ab short for address-book.

;; Some common scalars that we'll use in subsequent tests
(sp/scalar :scalar/string  string? ::sp/description "Non Blank String")
(sp/scalar :scalar/boolean boolean?)
(sp/scalar :scalar/integer integer?)

(sp/enum   :scalar/au-state [:act :nsw :nt :qld :sa :tas :vic :wa]
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
                         :wa  "WA"})

;;; Common Attributes

(sp/attribute :ab/unit-no   :scalar/string   ::sp/label "Unit No")
(sp/attribute :ab/street-no :scalar/string   ::sp/label "Street No")
(sp/attribute :ab/street    :scalar/string   ::sp/label "Street")
(sp/attribute :ab/state     :scalar/au-state ::sp/label "State")

(sp/entity :ab/address
           [:ab/unit-no
            :ab/street-no
            :ab/street
            :ab/state]
           ::sp/required-keys [:ab/state]
           ::sp/label         "Address"
           ::sp/description   "An Australian Address")

(deftest scalars
  (testing "Basic Usage"
    (is (= (sp/description :scalar/string)))
    (is (sp/scalar? :scalar/string))
    (is (s/valid? :scalar/string "asd"))))

(deftest enums
  (testing "Basic Usage"
    (is (sp/scalar? :scalar/au-state))
    (is (= (sp/values        :scalar/au-state) [:act :nsw :nt :qld :sa :tas :vic :wa]))
    (is (= (sp/labels        :scalar/au-state) {:act "Australian Capital Territory", :nsw "New South Wales", :nt "Northern Territory", :qld "Queensland", :sa "South Australia", :tas "Tasmania", :vic "Victoria", :wa "Western Australia"}))
    (is (= (sp/abbreviations :scalar/au-state) {:act "ACT", :nsw "NSW", :nt "NT", :qld "QLD", :sa "SA", :tas "TAS", :vic "VIC", :wa "WA"}))))

(deftest attributes
  (testing "Basic Usage"
    (is (= (sp/label :ab/street-no) "Street No"))
    (is (s/valid? :ab/street-no "asdf"))

    (is (= (sp/label :ab/state) "State"))
    (is (s/valid? :ab/state :qld))))

(deftest entities
  (testing "Basic Usage"
    (is (= (sp/label       :ab/address) "Address"))
    (is (= (sp/description :ab/address) "An Australian Address"))
    ;;
    (is (= (sp/attribute-keys :ab/address) [:ab/unit-no
                                            :ab/street-no
                                            :ab/street
                                            :ab/state]))
    (is (nil? (sp/identity-keys :ab/address)))
    (is (= (sp/required-keys :ab/address) [:ab/state]))))

;;;

(sp/attribute :ab/person-id   :scalar/string)
(sp/attribute :ab/given-name  :scalar/string)
(sp/attribute :ab/family-name :scalar/string)

(sp/entity :ab/person
           [:ab/person-id
            :ab/given-name
            :ab/family-name]
           ::sp/identity-keys [:ab/person-id]
           ::sp/required-keys [:ab/family-name])

(sp/entity-token  :ab/person-token  :ab/person)
(sp/entity-values :ab/person-values :ab/person)

(deftest tokens-and-values
  (is (= (sp/identity-keys :ab/person-token)
         [:ab/person-id]))

  (is (= (sp/attribute-keys :ab/person-token)
         [:ab/person-id]))

  (is (= (sp/value-keys :ab/person-token)
         nil))

  (is (= (sp/identity-keys :ab/person-values)
         nil))

  (is (= (sp/attribute-keys :ab/person-values)
         [:ab/given-name :ab/family-name]))

  (is (= (sp/value-keys :ab/person-values)
         [:ab/given-name :ab/family-name])))
