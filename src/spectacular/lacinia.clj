(ns spectacular.lacinia
  (:require [clojure.string :as s]
            [clojure.set :refer [union difference]]
            ;;
            [camel-snake-kebab.core :as csk]
            [camel-snake-kebab.extras :as cske]
            ;;
            [spectacular.core :as sp]
            [spectacular.utils :refer [nsk?]]))

;;; --------------------------------------------------------------------------------
;;  Naming convention, policy, will make it configurable later.

(defn gql-type-key
  [k options]
  (or (::type options)               ; explicitly provided gql type
      (sp/-get k ::type)             ; gql type registered with entity
      ;;
      ;; Or if we are an attribute then our scalar may have a special
      ;; GQL type so check for that too, but otherwise use the
      ;; scalar type and NOT the attribute-key as the type
      (when (sp/attr? k)
        (let [scalar-key (sp/-get k ::sp/scalar-key)]
          (or (sp/-get scalar-key ::type)
              scalar-key)))
      ;;
      ;; Failing all of that we just use the original k
      k))

(defn gql-type-name
  [k {:keys [input?]}]
  (let [type-part    (csk/->PascalCaseString k)
        context-part (if input? "In" "")]
    (-> (str type-part context-part)
        keyword)))

;;;

(defn listify
  [gql-type {:keys [many? required?]}]
  (cond->> gql-type
    (or many? required?) (list 'non-null)
    many?                (list 'list)))

(defn field-type
  ([k] (-> k
           (gql-type-key  nil)
           (gql-type-name nil)))

  ([m k & [options]]
   (let [gql-type (-> k
                      (gql-type-key  options)
                      (gql-type-name options))]
     (assoc m :type (listify gql-type options)))))

(defn field-description
  ([k]
   (or (sp/-get k ::description)
       (sp/-get k ::sp/description)))
  ;;
  ([m k & [{:keys [description] :as options}]]
   (if-let [val (or description
                    (field-description k))]
     (assoc m :description val)
     m)))

;;;

(defn canonicalise-ref
  [ref]
  (let [{ref-type :type
         kind     :kind
         :as ref}
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
    (when-not (#{:token :values :record} (or kind :record))
      (throw (ex-info "Invalid reference kind; kind-type must be a :token :values or :record."
                      {:kind kind
                       :ref  ref})))
    ref))

(defn ref->field
  [ref & [{:keys [required?]}]]
  (let [{ref-type :type
         :keys    [resolver description]
         :as      ref}
        (merge (canonicalise-ref ref)
               ;;
               ;; Explicitly overridden to be required, or required?
               ;; has been provided as part of the original ref map.
               ;;
               ;; We need to do it here so that the field-type knows
               ;; to make it (non-null ...).
               (when (or required? (:required? ref))
                 {:required? true}))
        ;;
        field (cond
                (sp/exists? ref-type)
                (-> {}
                    (field-type        ref-type ref)
                    (field-description ref-type ref))
                ;;
                :else {:type (listify (csk/->PascalCaseKeyword ref-type)
                                      ref)})]
    (cond-> field
      resolver    (assoc :resolver    resolver)
      description (assoc :description description))))

;;; --------------------------------------------------------------------------------

(defn ref->field-name
  [k {:keys [has?]}]
  ;; FIXME: need to cater for gql-type on the options that overrides
  ;; the coercion to
  (let [k-name (name k)]
    (-> (if (s/ends-with? k-name "?")
          (str (if has? "has" "is")
               (-> k-name
                   (s/replace #"\?" "")
                   csk/->PascalCaseString))
          (csk/->camelCaseString k))
        keyword)))

(defn canonicalise-enum
  [enum]
  (let [enum (cond
               (sp/enum? enum)
               (-> {:type   enum
                    :values (sp/values enum)}
                   (field-description enum))
               ;;
               (map? enum) enum
               ;;
               :else (throw (ex-info "Invalid enum; must be (sp/enum? key) or map." {:enum enum})))]
    (when-not (and (:type   enum)
                   (:values enum))
      (throw (ex-info "Invalid enum; must contain :type and :values." {:enum enum})))
    enum))

(defn enum->enum
  [enum]
  (let [{enum-type :type
         :keys     [values description]
         :as       enum} (canonicalise-enum enum)]

    [(if (sp/enum? enum-type)
       (-> enum-type (gql-type-key  nil) (gql-type-name nil))
       (csk/->PascalCaseKeyword enum-type))
     ;;
     (-> (select-keys enum [:description])
         (assoc :values (mapv csk/->SCREAMING_SNAKE_CASE_KEYWORD values)))]))

;;; --------------------------------------------------------------------------------

(defn entity->output-fields
  [entity & [options]]
  (let [fields (cond
                 (sp/entity? entity)
                 (map (fn [k] [(ref->field-name k nil) (ref->field k)])
                      (sp/attribute-keys entity))

                 (map? entity)
                 (map (fn [[k v]] [(ref->field-name k nil) (ref->field v)])
                      entity))]
    (when (empty? fields)
      (throw (ex-info "Invalid entity; must be (sp/entity? entity) or be a map of fields." {:entity entity})))
    (into {} fields)))

(defn entity->input-fields
  [entity & [options]]
  (let [fields (cond
                 (sp/entity? entity)
                 (let [required? (union (-> entity sp/identity-keys set)
                                        (-> entity sp/required-keys set))]
                   (map (fn [k] [(ref->field-name k nil) (ref->field k {:required? (required? k)})])
                        (sp/attribute-keys entity)))

                 (map? entity)
                 (map (fn [[k v]] [(ref->field-name k nil) (ref->field v v)])
                      entity))]
    (when (empty? fields)
      (throw (ex-info "Invalid entity; must be (sp/entity? entity) or be a map of fields." {:entity entity})))
    (into {} fields)))

;;; --------------------------------------------------------------------------------

(defn endpoint->gql
  "Can be either a mutation or query."
  [{return-type :type
    :keys       [args description resolver]
    :as         endpoint}]
  (cond-> (ref->field return-type)
    args        (assoc :args        (->> args
                                         (map (fn [[k v]]
                                                [(ref->field-name k v)
                                                 (ref->field      v v)]))
                                         (into {})))
    description (assoc :description description)
    resolver    (assoc :resolver    resolver)))

(defn endpoints->gql
  [endpoints]
  (->> endpoints
       (mapv (fn [[k v]]
               [(ref->field-name k v)
                (endpoint->gql     v)]))))

;;; --------------------------------------------------------------------------------

(defn ref->object
  [ref]
  (let [{k :type :as ref} (canonicalise-ref ref)]
    (when-let [object-type (cond
                             (sp/enum?   k) :enum
                             (sp/entity? k) :entity)]
      (-> ref
          (assoc :object-type object-type)
          (dissoc :many? :required?)))))

(defn endpoint-input-objects
  [& endpoints]
  (some->> endpoints
           (apply merge)
           (map    second)
           (mapcat :args)
           (map #(-> % second ref->object))
           (remove nil?)
           seq
           (group-by (juxt :type :kind))
           (sort-by  first)
           (map      #(-> % second first))
           vec))

(defn endpoint-output-objects
  [& endpoints]
  (some->> endpoints
           (apply merge)
           (map #(-> % second :type ref->object))
           (remove   nil?)
           seq
           (group-by (juxt :type :kind))
           (sort-by  first)
           (map      #(-> % second first))
           vec))

(defn referenced-enums
  [entity-type]
  (some->> (sp/attribute-keys entity-type)
           (map sp/get-attribute-type)
           (filter sp/enum?)
           seq
           set))

(defn referenced-entities
  [entity-type]
  (let [child-entities #(->> (sp/attribute-keys %)
                             (map sp/get-attribute-type)
                             (filter sp/entity?)
                             set)
        parent #{entity-type}]
    (loop [seen   #{}
           unseen (difference (child-entities entity-type)
                              parent
                              seen)]
      (if-let [unseen-type (first unseen)]
        (let [seen (conj seen unseen-type)]
          (recur seen
                 (difference (union unseen
                                    (child-entities unseen-type))
                             seen)))
        (some-> seen seq set)))))

;;; --------------------------------------------------------------------------------

#_
(defn transform-schema
  [{:keys [enums objects queries mutations]}]
  (let [output-objects (lc/endpoint-output-objects queries mutations)
        all-objects    (mapcat (referenced-objects output-objects))
        ;;
        input-objects  (lc/endpoint-input-objects  queries mutations)
        ;;
        enums        (some->> (concat enums
                                      (lc/endpoint-enums   queries mutations)
                                      (lc/referenced-enums output-objects)
                                      (lc/referenced-enums input-objects))
                              seq
                              (group-by :type)
                              (sort-by first)
                              (map #(-> second first)))

        ;;

        ]
    {:enums ()}
    )
  )
