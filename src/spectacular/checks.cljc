(ns spectacular.checks
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as st]
            ;;
            [coerce.core :refer [coerce]]
            ;;
            [spectacular.core :as sp]))

;;; --------------------------------------------------------------------------------

(def +checks+ (atom {}))

(defn register-check
  [k & {:as check}]
  (swap! +checks+ #(assoc % k check)))

(def +absent?+  nil)
(def +present?+ nil)

(defn register-presence-fns
  [present? absent?]
  ;; FIXME: Maybe do these with a protocol?
  (alter-var-root (var +absent?+)  absent?)
  (alter-var-root (var +present?+) present?))

;;; --------------------------------------------------------------------------------

(defn format-entity-identity
  [entity-type entity]
  (str entity-type "{"
       (->> (sp/identity-keys entity-type)
            (map (fn [k] (str k ":" (-> (get entity k) (coerce :string)))))
            (st/join " "))
       "}"))

(defn check-present
  [entity-type entity]
  (when (+absent?+ entity-type entity)
    (str "Entity " (format-entity-identity entity-type entity) " must be present.")))

(defn check-absent
  [entity-type entity]
  (when (+present?+ entity-type entity)
    (str "Entity " (format-entity-identity entity-type entity) " must be absent.")))

(defn check-entity
  [entity-type record]
  (when-not (s/valid? entity-type record)
    (s/explain-data entity-type record)))

(defn entity-check
  [check-type & {:keys [entity-type check-content exists?]}]
  (let [check-presence (cond
                         (true?  exists?)  #(check-present entity-type %)
                         (false? exists?)  #(check-absent  entity-type %))]
    (register-check check-type
                   :check-values  #(check-entity entity-type %)
                   :check-content (cond
                                    (and check-presence check-content)
                                    #(or (check-presence %)
                                         (check-content  %))
                                    :else (or check-presence check-content)))))

(defn make-check-entity-values
  "Makes a function that checks the values against the entity type."
  [entity-type]
  #(check-entity entity-type %))

;;; --------------------------------------------------------------------------------

(defn -check-command
  [{:keys [check-values check-content] :as check}
   {:keys [content] :as command}]
  (or (when-let [errors (check-values content)]
        {:error-type :values
         :errors     errors})
      (when-let [errors (and check-content (check-content content))]
        {:error-type :content
         :errors     errors})))

(defn check-command
  [{:keys [command-type] :as command}]
  (if-let [check (get @+checks+ command-type)]
    (-validate check command)
    {:error-type :unknown-command
     :command    command
     :message    (str "Couldn't find a check for command-type: " command-type)}))
