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
(sp/scalar :s/keyword keyword?)
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

(sp/attribute :a/unit-no       :s/string   ::sp/label "Unit No"       ::sp/optional? true)
(sp/attribute :a/building-name :s/string   ::sp/label "Building Name" ::sp/optional? true)
(sp/attribute :a/street-no     :s/string   ::sp/label "Street No")
(sp/attribute :a/street        :s/string   ::sp/label "Street")
(sp/attribute :a/state         :s/au-state ::sp/label "State")

(sp/entity :e/address
           [:a/unit-no
            :a/building-name
            :a/street-no
            :a/street
            :a/state]
           ::sp/label         "Address"
           ::sp/description   "An Australian Address")

(sp/attribute :a/integers     :s/integer ::sp/label "Integers"     ::sp/set?    true ::sp/optional? true)
(sp/attribute :a/strings      :s/string  ::sp/label "Strings"      ::sp/vector? true ::sp/optional? true)
(sp/attribute :a/string-tags  :s/string  ::sp/label "String Tags"  ::sp/set?    true)
(sp/attribute :a/keyword-tags :s/keyword ::sp/label "Keyword Tags" ::sp/set?    true)
(sp/attribute :a/addresses    :e/address ::sp/label "Addresses"    ::sp/vector? true ::sp/min-count 2 ::sp/max-count 5)

(deftest scalars
  (testing "Basic Usage"
    (is (= (sp/description :s/string) "Non Blank String"))
    (is (sp/scalar? :s/string))
    (is (s/valid? :s/string "asd"))
    (is (-> (s/valid? :s/string 12) not))))

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
    (is (s/valid? :a/state :qld)))

  (testing "Nilable Attributes"
    (is (= (sp/label :a/unit-no) "Unit No"))
    (is (s/valid? :a/unit-no "asdf"))
    (is (s/valid? :a/unit-no nil)))

  (testing "Basic Set/Vector Usage"
    (is (s/valid? :a/integers #{1 2 3}))
    (is (s/valid? :a/integers nil))

    (is (s/valid? :a/strings ["one" "two" "three"]))
    (is (s/valid? :a/strings nil))

    (is (sp/attr? :a/string-tags))
    (is (s/valid? :a/string-tags #{"asd" "blah"})))

  (testing "Entity Set/Vector Usage"
    (is (sp/attr? :a/addresses))
    (is (s/valid? :a/addresses [{:a/unit-no       nil
                                 :a/building-name nil
                                 :a/street-no     "3"
                                 :a/street        "Smith St"
                                 :a/state         :nsw}
                                {:a/unit-no       nil
                                 :a/building-name nil
                                 :a/street-no     "9"
                                 :a/street        "Smith St"
                                 :a/state         :nsw}]))

    (is (-> (s/valid? :a/addresses [{:a/unit-no       nil
                                     :a/building-name nil
                                     :a/street-no     "3"
                                     :a/street        "Smith St"
                                     :a/state         :nsw}])
            not))))

(deftest entities
  (testing "Basic Usage"
    (is (= (sp/label       :e/address) "Address"))
    (is (= (sp/description :e/address) "An Australian Address"))
    ;;
    (is (= (sp/attribute-keys :e/address) [:a/unit-no
                                           :a/building-name
                                           :a/street-no
                                           :a/street
                                           :a/state]))
    (is (nil? (sp/identity-keys :e/address))))

  (testing "Validity"
    (is (s/valid? :e/address {:a/unit-no       nil
                              :a/building-name nil
                              :a/street-no     "1"
                              :a/street        "Smith"
                              :a/state         :nsw}))

    (is (s/valid? :e/address {:a/building-name nil
                              :a/unit-no       nil
                              :a/street-no     "1"
                              :a/street        "Smith"
                              :a/state         :nsw}))

    (is (s/valid? :e/address {:a/building-name "The Manor"
                              :a/unit-no       "B"
                              :a/street-no     "1"
                              :a/street        "Smith"
                              :a/state         :nsw}))))

;;;

(sp/attribute :a/person-id   :s/string)
(sp/attribute :a/given-name  :s/string)
(sp/attribute :a/family-name :s/string)

(sp/entity :e/person
           [:a/person-id
            :a/given-name
            :a/family-name]
           ::sp/identity-keys [:a/person-id])

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

;;; --------------------------------------------------------------------------------
;;  Snippets to help wit debugging below here

(comment
  (do
    (sp/clear! :s :a :e)
    (sp/scalar :s/string  string? ::sp/description "Non Blank String")
    (sp/scalar :s/keyword keyword?)
    (sp/scalar :s/boolean boolean?)
    (sp/scalar :s/integer integer?)

    (macroexpand '(sp/attribute :a/integers :s/integer ::sp/label "Integers" ::sp/set? true ::sp/optional? true))))
