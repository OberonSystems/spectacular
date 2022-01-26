(ns spectacular.utils
  (:require [clojure.string :as s]
            [camel-snake-kebab.core :as csk]))

;;;

(defn keyword->label
  [k]
  (-> (csk/->Camel_Snake_Case_String k)
      (s/replace "_" " ")
      s/trim))

(defn hash-map*
  [& [head & tail :as params]]
  (let [[head tail] (if (-> params count odd?)
                [head tail]
                [{} params])]
   (->> tail
        (partition 2)
        (filter second)
        (map vec)
        (into head))))

(defn key->as
  "Merges the key of the map entry into the values with a key of 'as'."
  [[k m] as]
  (assoc m as k))
