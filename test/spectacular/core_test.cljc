(ns spectacular.core-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]])
  (:require [spectacular.core :as sp]
            :reload))

(sp/clear! :s :a :e)

;; Some common scalars that we'll use in subsequent tests
(sp/scalar :s/string  string? ::sp/description "Non Blank String")
(sp/scalar :s/boolean boolean?)
(sp/scalar :s/integer integer?)

(sp/enum   :s/au-state [:act :nsw :nt :qld :sa :tas :vic :wa]
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

(sp/attribute :a/unit-no   :s/string   ::sp/label "Unit No")
(sp/attribute :a/street-no :s/string   ::sp/label "Street No")
(sp/attribute :a/street    :s/string   ::sp/label "Street")
(sp/attribute :a/state     :s/au-state ::sp/label "State")

(sp/entity :e/address
           [:a/unit-no
            :a/street-no
            :a/street
            :a/state]
           ::sp/required-keys [:a/state]
           ::sp/label         "Address"
           ::sp/description   "An Australian Address")

(deftest scalars
  (testing "Basic Usage"
    (is (= (sp/description :s/string)))
    (is (sp/scalar? :s/string))
    (is (s/valid? :s/string "asd"))))

(deftest enums
  (testing "Basic Usage"
    (is (sp/scalar? :s/au-state))
    (is (= (sp/values        :s/au-state) [:act :nsw :nt :qld :sa :tas :vic :wa]))
    (is (= (sp/labels        :s/au-state) {:act "Australian Capital Territory", :nsw "New South Wales", :nt "Northern Territory", :qld "Queensland", :sa "South Australia", :tas "Tasmania", :vic "Victoria", :wa "Western Australia"}))
    (is (= (sp/abbreviations :s/au-state) {:act "ACT", :nsw "NSW", :nt "NT", :qld "QLD", :sa "SA", :tas "TAS", :vic "VIC", :wa "WA"}))))

(deftest attributes
  (testing "Basic Usage"
    (is (= (sp/label :a/street-no) "Street No"))
    (is (s/valid? :a/street-no "asdf"))

    (is (= (sp/label :a/state) "State"))
    (is (s/valid? :a/state :qld))))

(deftest entities
  (testing "Basic Usage"
    (is (= (sp/label       :e/address) "Address"))
    (is (= (sp/description :e/address) "An Australian Address"))
    ;;
    (is (= (sp/attribute-keys :e/address) [:a/unit-no
                                           :a/street-no
                                           :a/street
                                           :a/state]))
    (is (nil? (sp/identity-keys :e/address)))
    (is (= (sp/required-keys :e/address) [:a/state]))))

;;;

(sp/attribute :a/person-id   :s/string)
(sp/attribute :a/given-name  :s/string)
(sp/attribute :a/family-name :s/string)

(sp/entity :e/person
           [:a/person-id
            :a/given-name
            :a/family-name]
           ::sp/identity-keys [:a/person-id]
           ::sp/required-keys [:a/family-name])

(sp/entity-token  :e/person-token  :e/person)
(sp/entity-values :e/person-values :e/person)

(deftest tokens-and-values
  (is (= (sp/identity-keys :e/person-token)
         [:a/person-id]))

  (is (= (sp/attribute-keys :e/person-token)
         [:a/person-id]))

  (is (= (sp/value-keys :e/person-token)
         nil))

  (is (= (sp/identity-keys :e/person-values)
         nil))

  (is (= (sp/attribute-keys :e/person-values)
         [:a/given-name :a/family-name]))

  (is (= (sp/value-keys :e/person-values)
         [:a/given-name :a/family-name])))
