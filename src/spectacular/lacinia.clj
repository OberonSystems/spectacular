(ns spectacular.lacinia
  (:require [clojure.string :as s]
            [clojure.set :refer [union difference intersection]]
            ;;
            [camel-snake-kebab.core :as csk]
            [camel-snake-kebab.extras :as cske]
            ;;
            [spectacular.core :as sp]
            [spectacular.utils :refer [hash-map* key->as]]))

(defn wrap-with-type-hints
  [gql-type & {:keys [many? optional?]}]
  (cond->> gql-type
    (or many? (not optional?)) (list 'non-null)
    many?                      (list 'list)))

;;; --------------------------------------------------------------------------------

(defn- -description
  [k & [description]]
  (or description
      (sp/-get k ::description)
      (sp/-get k ::sp/description)))

(defn -attr-value
  [ref-type value-key]
  ;; Just look the 'thing' up on the ref-type which can be anything.
  (or (sp/-get ref-type value-key)
      ;; Special case handling just in case it's an attribute, in
      ;; which case we also check it.  This is generally what we want.
      (some-> ref-type
              sp/get-attribute-type
              (sp/-get value-key))))

(defn gql-type-name
  [k]
  (csk/->PascalCaseKeyword k))

(defn gql-field-name
  [k]
  (csk/->camelCaseKeyword k))

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

          :else (throw (ex-info "Invalid reference" {:ref ref})))]
    (when-not (or (-> ref-type sp/exists?)
                  (-> ref-type namespace nil?))
      (throw (ex-info "Invalid reference type; ref-type must be a register spectacular keyword or an unnamespaced keyword."
                      {:ref-type ref-type
                       :ref      ref})))
    (if (sp/attr? ref-type)
      (assoc ref
             :many?     (or (sp/attr-set? ref-type) (sp/attr-vector? ref-type))
             :optional? (sp/attr-optional? ref-type))
      ref)))

(defn enum-ref?
  [ref]
  (-> ref :type sp/enum?))

(defn entity-ref?
  [ref]
  (-> ref :type sp/entity?))

;;; --------------------------------------------------------------------------------
;;  Naming convention, policy, will make it configurable later.

(defn ref-type->field-name
  [ref-type]
  (-> (or (sp/-get ref-type ::name)
          ref-type)
      gql-field-name))

(defn ref->field-type
  [{ref-type :type :as ref} &
   {:keys [in?]}]
  ;; If ref-type is a registered spec then we need to check for
  ;; registered gql mappings, either directly or for attributes then
  ;; for it's associated scalar.
  (let [ref-type (cond
                   (sp/attr? ref-type)
                   (let [attribute-type (sp/-get ref-type ::sp/attribute-type)]
                     ;; Check for an lacinia specialised type first,
                     ;; or the attribute-type otherwise.
                     (or (sp/-get attribute-type ::type)
                         attribute-type))
                   ;;
                   (sp/exists? ref-type)
                   (or (sp/-get ref-type ::type)
                       ref-type)
                   ;;
                   :else ref-type)]
    (-> (or (when (and in? (sp/entity? ref-type))
              (str (name ref-type) "-in"))
            ref-type)
        gql-type-name)))

(defn ref->field
  [{ref-type :type
    :keys    [description optional? many? resolve]
    :as      ref}
   & {:keys [in?]}]
  ;; (println ref-type optional?)
  (hash-map* :type        (-> ref
                              (ref->field-type      :in?       in?)
                              (wrap-with-type-hints :many?     many?
                                                    :optional? (if in?
                                                                 optional?
                                                                 true)))
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
  [(gql-type-name enum-type)
   (hash-map* :values      (mapv csk/->SCREAMING_SNAKE_CASE_KEYWORD values)
              :description description)])

;;; --------------------------------------------------------------------------------

(defn entity-ref->object
  [{entity-type :type :as ref}
   & {:keys [in? edges]}]
  (let [attribute-fields (->> (concat (sp/attribute-keys entity-type)
                                      (-attr-value entity-type (if in? ::input-attributes ::output-attributes)))
                              (map canonicalise-ref)
                              (map (fn [{ref-type :type :as ref}]
                                     [(ref-type->field-name ref-type)
                                      (ref->field ref :in? in?)]))
                              (into {}))
        edge-fields       (when-not in?
                            ;; Edge fields cannot be used with
                            ;; input-objects.
                            (some->> edges
                                     (map (fn [[field ref]]
                                            (let [{ref-type :type :as ref} (canonicalise-ref ref)]
                                              [(ref-type->field-name field)
                                               (ref->field ref)])))
                                     (into {})))]
    ;; When generating objects from entities we use the Entity Type in
    ;; the "name" position.
    [(ref->field-type ref :in? in?)
     (hash-map* :fields      (merge attribute-fields edge-fields)
                :description (-description entity-type))]))

(defn field-def->field
  [{:keys [description] :as field-def}
   & {:keys [in?]}]
  (cond
    (or (sp/exists?  field-def)
        (entity-ref? field-def)
        ;; Plain Keywords are just defined with the schema for GQL only.
        (keyword?    field-def))
    (hash-map* (-> (canonicalise-ref field-def)
                   (ref->field :in? in?))
               :description description)
    ;;
    (map? field-def)
    (hash-map* :type        (-> field-def :type gql-type-name)
               :description (-> field-def :description))))

(defn fields->fields
  [fields & {:keys [in?]}]
  (some->> fields
           (map (fn [[field-name field-def]]
                  [(gql-field-name field-name)
                   (field-def->field field-def :in? in?)]))
           seq
           (into {})))

(defn object->object
  [{object-type :type
    :keys [fields description]}
   & {:keys [in?]}]
  ;;
  [(gql-type-name object-type)
   (hash-map* :fields      (fields->fields fields :in? in?)
              :description description)])

(defn endpoint->endpoint
  "Can be either a mutation or query."
  [{return-type :type
    :keys       [args description resolve]
    :as         endpoint}]
  (hash-map* :type        (let [{return-type :type
                                 :keys [many?]
                                 :as ref} (canonicalise-ref return-type)]
                            (-> ref
                                (ref->field-type      :in?   false)
                                (wrap-with-type-hints :many? many? :optional? true)))
             :args        (fields->fields args :in? true)
             :description description
             :resolve     resolve))

(defn endpoints->endpoints
  [endpoints]
  (some->> endpoints
           (map (fn [[endpoint-name endpoint-def]]
                  [(gql-field-name endpoint-name)
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
             endpoints))

  (defn object-fields->refs
    [objects]
    (refs-fn (fn [{:keys [fields]}]
               (map (fn [[_ v]] (canonicalise-ref v))
                    fields))
             objects)))

(defn unions->refs
  [unions]
  (some->> unions
           (mapcat second)
           distinct
           sort
           (mapv   canonicalise-ref)))

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
  [{entity-type :type :as ref}]
  (some->> (concat (sp/attribute-keys entity-type)
                   (-attr-value entity-type ::input-attributes)
                   (-attr-value entity-type ::output-attributes))
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

(defn edge-entity-refs
  [edges]
  (some->> edges
           (mapcat val)
           (map    second)
           (map    canonicalise-ref)
           (filter entity-ref?)
           (map    #(entity-ref->object % :edges (get edges (:type %))))))

(defn generate-schema
  [{:keys [enums objects unions edges input-objects
           queries mutations]}]
  (let [endpoints   (merge queries mutations)
        ;;
        output-refs (-> (concat (endpoint-types->refs endpoints)
                                (object-fields->refs  objects)
                                (unions->refs unions))
                        expand-to-include-referenced-entities)
        input-refs  (-> (concat (endpoint-args->refs endpoints)
                                (object-fields->refs input-objects))
                        expand-to-include-referenced-entities)
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
        ;;
        enums         (some->> (concat (map #(-> % (key->as :type) enum->enum) enums)
                                       (map enum-ref->enum enum-refs))
                               seq
                               (sort-by first)
                               (into    {}))
        ;;
        objects       (some->> (concat (map #(-> % (key->as :type) object->object)               objects)
                                       (map #(entity-ref->object % :edges (get edges (:type %))) output-entity-refs)
                                       (edge-entity-refs edges))
                               seq
                               (sort-by first)
                               (into    {}))
        input-objects (some->> (concat (map #(-> % (key->as :type) (object->object :in? true)) input-objects)
                                       (map #(entity-ref->object % :in? true)                  input-entity-refs))
                               seq
                               (sort-by first)
                               (into    {}))
        ;;
        unions        (some->> unions
                               (map (fn [[union-name members]]
                                      [(gql-type-name union-name)
                                       {:members (->> members sort (mapv gql-type-name))}]))
                               (into    {}))]
    (hash-map* :enums         enums
               :objects       objects
               :unions        unions
               :input-objects input-objects
               :queries       (endpoints->endpoints queries)
               :mutations     (endpoints->endpoints mutations))))

;;; --------------------------------------------------------------------------------
;;  Utils that can be used when configuring the LC integration

(def enum->gql csk/->SCREAMING_SNAKE_CASE_STRING)
(def enum->clj csk/->kebab-case-keyword)

(def enums->gql #(mapv csk/->SCREAMING_SNAKE_CASE_STRING %))
(def enums->clj #(mapv csk/->kebab-case-keyword %))

;;;

(defn gql->clj
  [m]
  ;; FIXME: this should also optionally take a map of args that can be
  ;; used for name translations, etc.
  (cske/transform-keys csk/->kebab-case-keyword m))

(defn gql->entity
  [entity-type record]
  ;; Need to coerce the names and the types, need to check with the
  ;; entity about doing extra conversions, like for enums.
  (->> (concat (sp/attribute-keys entity-type)
               (-attr-value entity-type ::input-attributes))
       (map (fn [ref-type]
              (let [field-name (ref-type->field-name ref-type)
                    ->clj      (or (-attr-value ref-type ::->clj) identity)]
                [ref-type (some-> (get record field-name)
                                  ->clj)])))
       (into {})))

(def name->keyword (memoize #(-> % name keyword)))

(defn entity->gql
  [entity-type record]
  ;; Need to coerce the names and the types, need to check with the
  ;; entity about doing extra conversions, like for enums.
  (->> (concat (sp/attribute-keys entity-type)
               (-attr-value entity-type ::output-attributes))
       (map (fn [ref-type]
              (let [value (or (get record ref-type)
                                   ;; If we don't find an namespaced
                                   ;; variant we can go for a plain
                                   ;; keyword.  This is often useful
                                   ;; for getting things from maps
                                   ;; returned from databases.
                                   (get record (name->keyword ref-type)))]
                (when-not (nil? value)
                (let [field-name (ref-type->field-name ref-type)
                      ->gql      (or (-attr-value ref-type ::->gql) identity)]
                   [field-name (some-> value ->gql)])))))
       (into {})))
