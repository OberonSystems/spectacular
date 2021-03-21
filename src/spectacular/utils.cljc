(ns spectacular.utils
  (:require [clojure.string :as s]
            [camel-snake-kebab.core :as csk]))

;;;

(defn keyword->label
  [k]
  (-> (csk/->Camel_Snake_Case_String k)
      (s/replace "_" " ")
      s/trim))
