(ns spectacular.utils
  (:require [clojure.string :as s]
            [camel-snake-kebab.core :as csk]))

;;;

(defn keyword->label
  [k]
  (-> (csk/->Camel_Snake_Case_String k)
      (s/replace "_" " ")
      s/trim))

(def ns-keyword->keyword
  (memoize #(-> % name keyword)))

(defn hash-map*
  [& [head & tail :as params]]
  (let [[head tail] (if (-> params count odd?)
                [head tail]
                [{} params])]
   (->> tail
        (partition 2)
        (remove #(-> % second nil?))
        (map vec)
        (into head))))

(defn key->as
  "Merges the key of the map entry into the values with a key of 'as'."
  [[k m] as]
  (assoc m as k))
