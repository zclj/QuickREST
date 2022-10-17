(ns quickrest.alpha.resources.schema
  (:require [malli.core :as m]
            [malli.util :as mu]
            [xtdb.api :as xt]))

(defn make-schema-node
  ([id type name components]
   (make-schema-node id type name components false))
  ([id type name components root?]
   (cond-> {:schema.node/id         id
            :schema.node/type       type
            :schema.node/name       name
            :schema.node/components components}
     root? (assoc :schema.node/root root?))))

(def map-key-t :map/key)
(def map-k-id "map.key/Foo")

(def base-type :keyword)

(defn make-vector-type-name
  [entry]
  (if (keyword? entry)
    (if (qualified-keyword? entry)
      (str (namespace entry) "/" (name entry))
      (name entry))
    entry))

(defn make-entry-component-names
  [[k v]]
  (let [v-entry (if (map? (second (m/form v)))
                     (nth (m/form v) 2)
                     (second (m/form v)))
        v-name  (if (vector? v-entry)
                     (name (first v-entry))
                     (name v-entry))
        v-name2 (if (= "vector" v-name)
                  (str v-name "->" (make-vector-type-name (second v-entry))
                       ;;(str (second v-entry))
                       )
                     v-name)]
    (str "map.entry/" k "::" v-name2)))

;; Map entry node must also produce the schema nodes of the entries
(defn make-entry-node
  "Produces:
   map.entry/Foo::string with components
     - map.entry.key/Foo
     - map.entry.value/string
  , in addition the entry nodes must be created:
   - map.key/Foo with component 'keyword'
   - map.value/string 'string'"
  [index [k v]]
  ;;(println (str "entry => " [k v]))
  (let [v-entry    (if (map? (second (m/form v)))
                     (nth (m/form v) 2)
                     (second (m/form v)))
        v-name     (if (vector? v-entry)
                     (name (first v-entry))
                     ;;(make-vector-type-name (first v-entry))
                     (name v-entry))
        v-name2    (if (= "vector" v-name)
                     (str v-name "->" (make-vector-type-name (second v-entry)))
                     v-name)
        entry-id   (str "map.entry/" k "::" v-name2)
        value-id   (str "map.value/" v-name2)
        key-id     (str "map.key/" k)
        entry-node (make-schema-node entry-id :map/entry k [key-id value-id])
        key-node   (make-schema-node key-id :map/key k ["keyword"])
        value-node (make-schema-node value-id :map/value v-name2 [v-name2])]
    (merge index {entry-id entry-node
                  key-id   key-node
                  value-id value-node})))

(defn make-map-node
  [{:keys [gen-name-f] :as _ctx} index schema]
  (let [new-index (reduce
                   (fn [acc entry]
                     (make-entry-node acc entry))
                   index
                   (m/entries schema))
        comps     (mapv #(make-entry-component-names %) (m/entries schema))
        map-name  (gen-name-f schema)]
    (assoc
     new-index map-name (make-schema-node map-name :container/map map-name comps
                                          (contains? (m/properties schema)
                                                     :schema.node/root)))))

(defn make-vector-node
  [{} index schema]
  (let [clean-schema   (mu/update-properties schema (fn [_]))
        vector-comp    (second (m/form clean-schema))
        vector-comp-id (if (keyword? vector-comp)
                         (if (qualified-keyword? vector-comp)
                           (str (namespace vector-comp) "/" (name vector-comp))
                           (name vector-comp))
                         vector-comp)
        vector-id      (str "vector->" vector-comp-id)
        vector-node    (make-schema-node
                     vector-id :container/vector "vector" [vector-comp-id]
                     (contains? (m/properties schema)
                                :schema.node/root))]
    (assoc index vector-id vector-node)))

(comment
  (make-map-node {} {:string "string"} [:map ["foo" :string]])
  )

(defn make-map-of-node
  [ctx index schema]
  ;;(println (str "Map of => " (m/form schema)))
  (let [clean-schema      (mu/update-properties schema (fn [_]))
        [_ k-type v-type] (m/form clean-schema)
        k-type-name       (if (keyword? k-type)
                            (name k-type)
                            k-type)
        v-type-name       (if (keyword? v-type)
                            (name v-type)
                            v-type)
        map-of-id         (str "map-of->" k-type-name "::" v-type-name)]
    (assoc index map-of-id
           (make-schema-node
            map-of-id :container/map-of "map-of" [k-type-name v-type-name]
            (contains? (m/properties schema)
                       :schema.node/root)))))

(defn make-ref-node
  [ctx index schema]
  ;;(println (str "ref => " (m/form schema)))
  (let [clean-schema   (mu/update-properties schema (fn [_]))
        [_ ref-value]  (m/form clean-schema)
        ref-value-name (make-vector-type-name ref-value)
        ref-id         (str "ref::" ref-value-name)]
    (assoc index ref-id
           (make-schema-node
            ref-id :reference "ref" [ref-value-name] (contains? (m/properties schema)
                                                                :schema.node/root)))))

(defn make-node
  [ctx index schema]
  ;;(println (str "Make node " (m/form schema) " with type " (m/type schema)))
  (cond
    (= :map (m/type schema))
    (make-map-node ctx index schema)
    (= :map-of (m/type schema))
    (make-map-of-node ctx index schema)
    (= :string (m/type schema))
    (assoc index "string" (make-schema-node "string" :string "Base type of string" []))
    (= :vector (m/type schema))
    (make-vector-node {} index schema)
    (= :int (m/type schema))
    (assoc index "int" (make-schema-node "int" :int "Base type of integer" []))
    (= :any (m/type schema))
    (assoc index "any" (make-schema-node "any" :any "Base type of any" []))
    (= :ref (m/type schema))
    (make-ref-node ctx index schema)
    :else
    (do
      ;;(assert false (str "Could not make node for " (m/type schema)))
      index)
    ))

(defn transform
  "'transform' takes an index of existing nodes and produces a new index with nodes
   from the given schema."
  [ctx index schema]
  (let [nodes-index      (atom index)
        processing-nodes (atom [])
        root-schema      (m/form schema ctx)
        ;;annotated-schema ;; (mu/update-properties
                         ;;  schema (fn [p] (assoc p :schema.node/root true)) ctx)
        ]
    (m/walk
     ;;annotated-schema
     schema
     (fn [schema _ _ _]
       (let [schema (if (= (m/form schema) root-schema)
                      (mu/update-properties
                       schema (fn [p] (assoc p :schema.node/root true)))
                      schema)]
         (reset! nodes-index (make-node ctx @nodes-index schema))
         (swap! processing-nodes conj schema)))
     ctx)
    @nodes-index))

(defn transform-definition
  [ctx definition]
  (assoc
   definition
   :schema/nodes
   (transform
    (merge
     ctx
     {:gen-name-f (fn [_]
                    (let [def-name-k (:data/key definition)]
                      (str (namespace def-name-k) "/" (name def-name-k))))})
    {}
    (:data/schema definition))))

(defn assoc-definition-registry-entry
  [registry definition]
  (assoc registry (:data/key definition) (:data/schema definition)))

(defn make-definitions-registry
  [definitions]
  (reduce assoc-definition-registry-entry {} definitions))

(defn transform-definitions
  [definitions]
  (let [definition-registry (make-definitions-registry definitions)
        registry            (merge (m/default-schemas) definition-registry)]
    (into #{} (mapv #(transform-definition {:registry registry} %) definitions))))

(defn make-operation-schema-node
  [amos-op]
  (let [params-ids
        (mapv
         :schema.node/id
         (filter
          (fn [m]
            (:schema.node/root m))
          (mapcat #(vals (:schema/nodes %)) (:operation/parameters amos-op))))
        resps-ids
        (mapv
         :schema.node/id
         (filter
          (fn [m]
            (:schema.node/root m))
          (mapcat #(vals (:schema/nodes %)) (:operation/responses amos-op))))
        op-name   (:info/name (:operation/info amos-op))
        resps-id  (str "responses/" op-name)
        params-id (str "parameters/" op-name)
        op-id     (str "operation/" op-name)]
    {resps-id  (make-schema-node resps-id
                                 :responses
                                 (str op-name " responses")
                                 resps-ids)
     params-id (make-schema-node params-id
                                 :parameters
                                 (str op-name " parameters")
                                 params-ids)
     op-id     (make-schema-node op-id
                                 :operation
                                 op-name
                                 [resps-id params-id])}))

(defn transform-operation
  [ctx {:keys [operation/parameters] :as amos-op}]
  (let [transform-schemas (fn [xs]
                            (mapv #(assoc % :schema/nodes
                                          (transform ctx
                                                     {}
                                                     (:data/schema %))) xs))
        op-with-schemas   (-> amos-op
                            (update :operation/parameters
                                    (fn [params] (transform-schemas params)))
                            (update :operation/responses
                                    (fn [resps] (transform-schemas resps))))]
    (assoc op-with-schemas :schema/nodes (make-operation-schema-node op-with-schemas))))

(defn transform-operations
  [ctx operations]
  (into #{} (mapv #(transform-operation ctx %) operations)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To AMOS

(defn merge-schema-nodes
  [{:amos/keys [definitions operations] :as amos}]
  (let [definition-registry (make-definitions-registry definitions)
        registry            (merge (m/default-schemas) definition-registry)
        ctx                 {:gen-name-f (fn [_] (random-uuid))
                             :registry   registry}]
    (-> amos
        (update :amos/operations (partial transform-operations ctx))
        (update :amos/definitions (partial transform-definitions)))))

(defn collect-schema-nodes
  [{:amos/keys [definitions operations] :as _amos}]
  (reduce
   (fn [acc op]
     (let [rsp-nodes   (apply merge (mapv :schema/nodes (:operation/responses op)))
           param-nodes (apply merge (mapv :schema/nodes (:operation/parameters op)))
           def-nodes   (apply merge (mapv :schema/nodes definitions))]
       (merge acc (:schema/nodes op) rsp-nodes param-nodes def-nodes)))
   {}
   operations))

(defn schema-nodes->xt
  [schema-nodes-index]
  (reduce-kv
   (fn [acc _ {:keys [schema.node/id] :as v}]
     (conj acc (assoc v :xt/id id)))
   []
   schema-nodes-index))

(defn schema-nodes->tx
  [schema-nodes]
  (mapv (fn [node] [::xt/put node]) schema-nodes))

;;;;
;; naming

(def map-type :container/map)
(def map-id "container.map/ProductName")

(defn make-map-graph-node
  [name schema]
  (let [components :foo]))

(m/entries [:map ["foo" [:string {:min 1}]]])
(->> (m/entries [:map ["foo" :string]])
    (mapv str))
(m/entries [:map ["foo" [:vector [:string {:min 1}]]]])

(def map-entry-type :map/entry)
(def map-entry-id "map.entry/ProductName::string")

(def map-key-type :map.entry/key)
(def map-key-id "map.entry.key/ProductName")

(def map-value-type :map.entry/value)
(def map-value-id "map.entry.value/string")

(def base-type-type :string)
(def base-type-id "string")

(comment
  (m/entries
   [:map ["name" [:string]]])
  (m/type [:map [:foo :string]])
  (m/entries [:map [:foo :string]])

  (mu/transform-entries)

  (m/ast [:map ["foo" :string]])
  (transform [:map ["name" [:string]]])
  )

