(ns spectacular.lacinia
  (:require [clojure.string :as s]
            [clojure.set :refer [union difference intersection]]
            ;;
            [camel-snake-kebab.core :as csk]
            [camel-snake-kebab.extras :as cske]
            ;;
            [spectacular.core :as sp]
            [spectacular.utils :refer [nsk? hash-map* key->as]]))

(defn wrap-with-type-hints
  [gql-type & {:keys [many? required?]}]
  (cond->> gql-type
    (or many? required?) (list 'non-null)
    many?                (list 'list)))

;;; --------------------------------------------------------------------------------

(defn- -description
  [k & [description]]
  (or description
      (sp/-get k ::description)
      (sp/-get k ::sp/description)))

;;; --------------------------------------------------------------------------------

(defn canonicalise-ref
  [ref]
  (let [{ref-type :type :as ref}
        (cond
          ;; :keyword
          (keyword? ref) {:type ref}

          ;; [:keyword]
          (and (vector? ref)
               (-> ref count (= 1))
               (-> ref first keyword?))
          {:type  (-> ref first)
           :many? true}

          ;; {:type :keyword}
          (and (map? ref)
               (-> ref :type keyword?))
          ref

          ;; {:type [:keyword]}
          (and (map? ref)
               (-> ref :type vector?)
               (-> ref :type count (= 1)))
          (assoc ref
                 :type  (-> ref :type first)
                 :many? true)

          :else (throw (ex-info "Invalid reference"
                                {:ref ref})))]
    (when-not (or (-> ref-type sp/exists?)
                  (-> ref-type namespace nil?))
      (throw (ex-info "Invalid reference type; ref-type must be a register spectacular keyword or an unnamespaced keyword."
                      {:ref-type ref-type
                       :ref      ref})))
    ref))

(defn enum-ref?
  [ref]
  (-> ref :type sp/enum?))

(defn entity-ref?
  [ref]
  (-> ref :type sp/entity?))

;;; --------------------------------------------------------------------------------
;;  Naming convention, policy, will make it configurable later.

(defn ref->field-type
  [{ref-type :type :as ref} &
   {:keys [in?]}]

  ;; If ref-type is a registered spec then we need to check for
  ;; registered gql mappings, either directly or for attributes then
  ;; for it's associated scalar.
  (let [ref-type (cond
                   (sp/attr? ref-type)
                   (let [scalar-key (sp/-get ref-type ::sp/scalar-key)]
                     (or (sp/-get scalar-key ::type)
                         scalar-key))
                   ;;
                   (sp/exists? ref-type)
                   (or (sp/-get ref-type ::type)
                       ref-type)
                   ;;
                   :else ref-type)]
    (-> (or (when (and in? (sp/entity? ref-type))
              (str (name ref-type) "-in"))
            ref-type)
        csk/->PascalCaseKeyword)))

(defn ref->field-name
  [{ref-type :type :as ref}]
  (-> (or (sp/-get ref-type ::name)
          ref-type)
      csk/->camelCaseKeyword))

(defn ref->field
  [{ref-type :type
    :keys    [description many? resolve]
    :as      ref}
   & {:keys [in? required?]}]
  (hash-map* :type        (-> ref
                              (ref->field-type      :in? in?)
                              (wrap-with-type-hints :many?     many?
                                                    :required? required?))
             :description (or description
                              (sp/-get ref-type ::description)
                              (sp/-get ref-type ::sp/description))
             :resolve     resolve))

;;; --------------------------------------------------------------------------------

(defn canonicalise-enum
  [{enum-type :type :as enum}]
  (cond
    (sp/enum? enum)
    {:type enum}
    ;;
    (enum-ref? enum)
    enum
    ;;
    :else (throw (ex-info "Invalid enum-ref." {:enum enum}))))

(defn enum-ref->enum
  [{enum-type :type :as enum-ref}]
  [(ref->field-type enum-ref)
   (hash-map* :values      (->> (sp/values enum-type)
                                (mapv csk/->SCREAMING_SNAKE_CASE_KEYWORD))
              :description (-description enum-type))])

(defn enum->enum
  [{enum-type :type
    :keys [values description] :as enum}]
  (when-not values
    (throw (ex-info "Invalid enum; must contain and :values." {:enum-type enum-type
                                                               :enum      enum})))
  [(csk/->PascalCaseKeyword enum-type)
   (hash-map* :values      (mapv csk/->SCREAMING_SNAKE_CASE_KEYWORD values)
              :description description)])

;;; --------------------------------------------------------------------------------

(defn entity-ref->object
  [{entity-type :type :as ref}
   & {:keys [in?]}]
  (let [required? (if in?
                    (union (-> entity-type sp/identity-keys set)
                           (-> entity-type sp/required-keys set))
                    #{})]
    ;; When generating objects from entities we use the Entity Type in
    ;; the "name" position.
    [(ref->field-type ref :in? in?)
     (hash-map* :fields     (->> (sp/attribute-keys entity-type)
                                 (map canonicalise-ref)
                                 (map (fn [{ref-type :type :as ref}]
                                        [(ref->field-name ref)
                                         (ref->field ref
                                                     :in?       in?
                                                     :required? (required? ref-type))]))
                                 (into {}))
                :description (-description entity-type))]))

(defn field-def->field
  [{:keys [description required?] :as field-def}
   & {:keys [in?]}]
  (cond
    (or (sp/exists?  field-def)
        (entity-ref? field-def))
    (hash-map* (-> (canonicalise-ref field-def)
                   (ref->field :in?       in?
                               :required? required?))
               :description description)
    ;;
    (keyword? field-def)
    {:type (csk/->PascalCaseKeyword field-def)}
    ;;
    (map? field-def)
    (hash-map* :type        (-> field-def :type csk/->PascalCaseKeyword)
               :description (-> field-def :description))))

(defn fields->fields
  [fields & {:keys [in?]}]
  (some->> fields
           (map (fn [[field-name field-def]]
                  [(csk/->camelCaseKeyword field-name)
                   (field-def->field field-def :in? in?)]))
           seq
           (into {})))

(defn object->object
  [{object-type :type
    :keys [fields description]}
   & {:keys [in?]}]
  ;;
  [(csk/->PascalCaseKeyword object-type)
   (hash-map* :fields      (fields->fields fields :in? in?)
              :description description)])

(defn endpoint->endpoint
  "Can be either a mutation or query."
  [{return-type :type
    :keys       [args description resolve]
    :as         endpoint}]
  (hash-map* :type        (let [{return-type :type
                                 :keys [many? required?]
                                 :as ref} (canonicalise-ref return-type)]
                            (-> ref
                                (ref->field-type      :in? false)
                                (wrap-with-type-hints :many?     many?
                                                      :required? required?)))
             :args        (fields->fields args :in? true)
             :description description
             :resolve     resolve))

(defn endpoints->endpoints
  [endpoints]
  (some->> endpoints
           (map (fn [[endpoint-name endpoint-def]]
                  [(csk/->camelCaseKeyword endpoint-name)
                   (endpoint->endpoint endpoint-def)]))
           seq
           (into {})))

;;; --------------------------------------------------------------------------------

(let [refs-fn (fn [extract-refs-fn endpoints]
                (some->> endpoints
                         (map      second)
                         (mapcat   extract-refs-fn)
                         (filter   #(or (enum-ref?   %)
                                        (entity-ref? %)))
                         seq
                         (group-by :type)
                         (sort-by  first)
                         (map      #(-> % second first))
                         vec))]
  (defn endpoint-types->refs
    [endpoints]
    (refs-fn #(-> % :type canonicalise-ref vector)
             endpoints))

  (defn endpoint-args->refs
    [endpoints]
    (refs-fn (fn [{:keys [args]}]
               (map (fn [[_ v]] (canonicalise-ref v))
                    args))
             endpoints)))

(defn referenced-entity-types
  [entity-ref-type ignore]
  (let [child-entities #(->> (sp/attribute-keys %)
                             (map sp/get-attribute-type)
                             (filter sp/entity?)
                             set)
        parent #{entity-ref-type}]
    (loop [seen   #{}
           unseen (difference (child-entities entity-ref-type)
                              parent
                              ignore
                              seen)]
      (if-let [unseen-type (first unseen)]
        (let [seen (conj seen unseen-type)]
          (recur seen
                 (difference (union unseen
                                    (child-entities unseen-type))
                             seen)))
        (some-> seen seq set)))))

(defn expand-to-include-referenced-entities
  [parent-refs]
  (let [parent-ref-types (->> parent-refs
                              (filter entity-ref?)
                              (map :type)
                              set)]
    (->> parent-ref-types
         (map #(referenced-entity-types % parent-ref-types))
         (apply union)
         (map canonicalise-ref)
         (into parent-refs))))

(defn referenced-enum-types
  [entity-type]
  (some->> (sp/attribute-keys entity-type)
           (map sp/get-attribute-type)
           (filter sp/enum?)
           seq
           set))

;;; --------------------------------------------------------------------------------

(defn throw-overlapping
  [type-name lc-values refs]
  (let [overlapping (intersection (->> lc-values keys set)
                                  (->> refs (map :type) set))]
    (when-not (empty? overlapping)
      (throw (ex-info (str "Can't generate schema as there are overlapping " type-name " definitions.")
                      {type-name overlapping})))))

(defn generate-schema
  [{:keys [enums objects input-objects
           queries mutations]}]
  (let [endpoints   (merge queries mutations)
        ;;
        output-refs (-> (endpoint-types->refs endpoints) expand-to-include-referenced-entities)
        input-refs  (-> (endpoint-args->refs  endpoints) expand-to-include-referenced-entities)
        ;;
        output-entity-refs (filter entity-ref? output-refs)
        _                  (throw-overlapping :objects objects output-entity-refs)

        input-entity-refs  (filter entity-ref? input-refs)
        _                  (throw-overlapping :input-objects input-objects input-entity-refs)
        ;;
        enum-refs (some->> (concat (filter enum-ref? output-refs)
                                   (filter enum-ref? input-refs)
                                   ;;
                                   (->> (concat output-entity-refs input-entity-refs)
                                        (mapcat referenced-enum-types)
                                        (map    canonicalise-enum)))
                           seq
                           (group-by :type)
                           (map      #(-> % second first)))
        _         (throw-overlapping :enums enums enum-refs)
        enums     (some->> (concat (map #(-> % (key->as :type) enum->enum) enums)
                               (map enum-ref->enum enum-refs))
                           seq
                       (sort-by first)
                       (into    {}))
        ;;
        objects (some->> (concat (map #(-> % (key->as :type) object->object) objects)
                                 (map entity-ref->object output-entity-refs))
                         seq
                         (sort-by first)
                         (into    {}))
        ;;
        input-objects (->> (concat (map #(object->object     % :in? true) input-objects)
                                   (map #(entity-ref->object % :in? true) input-entity-refs))
                           (sort-by first)
                           (into    {}))]
    (hash-map* :enums         enums
               :objects       objects
               :input-objects input-objects
               :queries       (endpoints->endpoints queries)
               :mutations     (endpoints->endpoints mutations))))
