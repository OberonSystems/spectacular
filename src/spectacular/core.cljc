(ns spectacular.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec.alpha :as s]))

(defonce +enums+ (atom {}))
(defn -set-enums
  [k m]
  (swap! +enums+ #(assoc % k m))
  nil)

(defmacro register-enum
  [k values & {:as options}]
  (let [values#  (set values)
        options# options]
    `(do
       (s/def ~k ~values#)
       (-set-enums ~k {:values  ~values#
                       :options ~options#}))))

(defn get-enum-values
  [k]
  (get-in @+enums+ [k :values]))

(defn get-enum-description
  [k]
  (get-in @+enums+ [k :options :description]))

;;;

(defonce +data-types+ (atom {}))
(defn -set-data-type
  [k m]
  (swap! +data-types+ #(assoc % k m))
  nil)

(defmacro register-data-type
  [k pred & {:as options}]
  (let [k#       k
        pred#    pred
        options# options]
    `(do
       (s/def ~k ~pred#)
       (-set-data-type ~k ~options#))))

(defn get-data-type
  [k]
  (get @+data-types+ k))

;;;

(defonce +field-types+ (atom {}))
(defn -set-field-type
  [k m]
  (swap! +field-types+ #(assoc % k m))
  nil)

(defmacro register-field
  [k data-type & {:as options}]
  (let [k#         k
        data-type# data-type
        options#   options]
    `(do
       (s/def ~k (s/get-spec ~data-type#))
       (-set-field-type ~k {:data-type ~data-type#
                            :options   ~options#}))))

(defn get-field
  [k]
  (get @+field-types+ k))

;;;

(defonce +entities+ (atom {}))
(defn -set-entity
  [k m]
  (swap! +entities+ #(assoc % k m))
  nil)

(defmacro register-entity
  [k field-ks & {:keys [optional?] :as options}]
  (let [k#         k
        field-ks#  field-ks
        optional?# optional?
        options#   options]
    `(do
       (s/def ~k (s/keys :req ~(->> field-ks (remove optional?#) vec)
                         :opt ~(->> field-ks (filter optional?#) vec)))
       (-set-entity ~k {:field-ks ~field-ks#
                        :options  ~options#}))))

(defn get-identity
  [k]
  (get-in @+entities+ [k :options :identity]))

(defn get-entity-description
  [k]
  (get-in @+entities+ [k :options :description]))

(defn get-entity-fields
  [k]
  (let [ks (get-in @+entities+ [k :field-ks])]
    (map get-field ks)))
