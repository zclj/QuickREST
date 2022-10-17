(ns quickrest.alpha.resources.amos
  (:require [quickrest.alpha.resources.xtdb :as rxt]
            [quickrest.alpha.utilities.schema :as usc]
            [clojure.tools.logging :as log]
            [malli.util :as mu]
            [quickrest.alpha.resource-access.schema :as ras]
            [ubergraph.alg :as ualg]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schemas

(def AMOSDomainMetaData
  [:map
   [:amos/domain
    [:map [:domain/meta-data :any]]]])

(def AMOSSchemaMapEntries
  [:sequential
   [:tuple
    [:string {:min 1}]
    [:vector :any]]])

(def AMOSSchemaMap
  [:vector :any])

(def AMOSSchema
  [:vector :any])

(def AMOSOperationResponse
  [:map
   [:data/schema :any]])

(def AMOSOperationResponsesSeq
  [:vector AMOSOperationResponse])

(def AMOSOperationResponses
  [:map
   [:operation/responses AMOSOperationResponsesSeq]])

(def AMOSOperationParameter
  [:map
   [:data/schema :any]])

(def AMOSOperationParametersSeq
  [:vector AMOSOperationParameter])

(def AMOSOperationParameters
  [:map
   [:operation/parameters AMOSOperationParametersSeq]])

(def AMOSOperationInfoEntry
  [:map
   [:info/name [:string {:min 1}]]
   [:info/key keyword?]])

(def AMOSOperationInfo
  [:map
   [:operation/info AMOSOperationInfoEntry]])

(def OperationId uuid?)

(def AMOSOperationId
  [:map
   [:operation/id OperationId]])

(def AMOSOperation
  [:map
   [:operation/id OperationId]
   [:operation/info AMOSOperationInfoEntry]
   [:operation/parameters {:optional true} AMOSOperationParametersSeq]
   [:operation/responses {:optional true} AMOSOperationResponsesSeq]])

(def AMOSOperationsValue
  [:set AMOSOperation])

(def AMOSOperationEntry
  [:amos/operations AMOSOperationsValue])

(def AMOSOperationsMap
  [:map AMOSOperationEntry])

(def AMOSDefinition
  [:map
   [:data/schema :any]
   [:data/name [:string {:min 1}]]
   [:data/key :keyword]])

(def AMOSDefinitionsValue
  [:set AMOSDefinition])

(def AMOSDefinitionsEntry
  [:amos/definitions AMOSDefinitionsValue])

(def AMOSDefinitionsMap
  [:map AMOSDefinitionsEntry])

(def AMOSIdValue uuid?)

(def AMOSIdEntry
  [:amos/id AMOSIdValue])

(def AMOSId
  [:map AMOSIdEntry])

(def AMOSNameValue [:string {:min 1}])

(def AMOSNameEntry
  [:amos/name AMOSNameValue])

(def AMOSName
  [:map AMOSNameEntry])

(def AMOS
  [:map
   AMOSIdEntry
   AMOSNameEntry
   [:amos/domain [:map [:domain/meta-data [:map]]]]
   AMOSDefinitionsEntry
   AMOSOperationEntry])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation

(defn make-amos-domain-meta-data
  [doc]
  {:amos/domain {:domain/meta-data doc}})

(def MakeAMOSOperationInput
  [:cat [:string {:min 1}] [:boolean] [:enum :string :int :boolean :double :any]])

(defn make-amos-operation-schema-key-value
  [k v required]
  (if required
    [k v]
    [k {:optional true} v]))

(defn make-amos-operation-parameter
  {:malli/schema [:=> MakeAMOSOperationInput AMOSOperationParameter]}
  [name required type]
  (let [map-entry (make-amos-operation-schema-key-value name type required)]
    {:data/schema    [:map map-entry]
     :parameter/name name}))

(defn make-amos-operation-parameters
  {:malli/schema [:=> [:cat AMOSOperationParametersSeq] AMOSOperationParameters]}
  [parameters]
  {:operation/parameters parameters})

(defn make-amos-operation-responses
  {:malli/schema [:=> [:cat AMOSOperationResponsesSeq] AMOSOperationResponses]}
  [responses]
  {:operation/responses responses})

(defn make-amos-operation-info
  {:malli/schema [:=> [:cat AMOSOperationInfoEntry] AMOSOperationInfo]}
  [info]
  {:operation/info info})

(defn make-amos-operation-id
  {:malli/schema [:=> [:cat OperationId] AMOSOperationId]}
  [id]
  {:operation/id id})

(defn make-amos-operations
  {:malli/schema [:=> [:cat AMOSOperationsValue] AMOSOperationsMap]}
  [operations]
  {:amos/operations operations})

(defn make-amos-definitions
  {:malli/schema [:=> [:cat AMOSDefinitionsValue] AMOSDefinitionsMap]}
  [defintions]
  {:amos/definitions defintions})

(defn make-amos-id
  {:malli/schema [:=> [:cat AMOSIdValue] AMOSId]}
  [id]
  {:amos/id id})

(defn make-amos-name
  {:malli/schema [:=> [:cat AMOSNameValue] AMOSName]}
  [name]
  {:amos/name name})

(defn make-definitions-index
  [{:keys [amos/definitions] :as _amos}]
  (reduce
   (fn [acc {:keys [data/schema data/key]}]
     (assoc acc key schema))
   {}
   definitions))

(defn status
  [{:amos/keys [operations definitions]}]
  {:amos/nrof-operations (count operations)
   :amos/nrof-definitions (count definitions)})

(defn definitions->entities
  [id-gen definitions]
  (mapv
   (fn [definition]
     (let [with-id (assoc definition :xt/id (id-gen definition))]
       (if-let [nodes (:schema/nodes definition)]
         (assoc with-id :schema/nodes (vec (keys nodes)))
         with-id)))
   definitions))

(defn operations->entities
  [operations]
  (mapv
   (fn [op]
     (cond-> op
       (:operation/id op) (assoc :xt/id (:operation/id op))
       (:schema/nodes op) (assoc :schema/nodes (vec (keys (:schema/nodes op))))))
   operations))

(defn ->XT-entities
  [id-gen {:amos/keys [domain definitions operations name id] :as amos}]
  (let [definition-entities (definitions->entities id-gen definitions)
        operation-entities  (operations->entities operations)]
    ;; what are the entites?
    ;; - AMOS, id, name etc
    ;; - ref: domain
    ;; - ref: defintions
    ;; - ref: operations
    (into
     (into [(-> (select-keys amos [:amos/domain :amos/name :amos/id])
                (assoc :xt/id id)
                (assoc :amos/definitions (mapv :xt/id definition-entities))
                (assoc :amos/operations (mapv :xt/id operation-entities)))]
           definition-entities)
     operation-entities)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query

(defn amoses
  [db pull]
  (rxt/q
   db
   {:find  [(list 'pull 'e pull)]
    :where [['e :amos/id]]}))

(defn domain
  [db amos-id]
  (->>
   (rxt/q
    db
    {:find  ['d]
     :in    ['id]
     :where [['e :amos/id 'id]
             ['e :amos/domain 'd]]}
    amos-id)
   ffirst))

(defn definitions
  [db amos-id pull]
  (rxt/q
   db
   {:find  [(list 'pull 'd pull)]
    :in    ['id]
    :where [['a :amos/id 'id]
            ['a :amos/definitions 'd]]}
   amos-id))

(defn data
  [db data-k pull]
  (rxt/q
   db
   {:find  [(list 'pull 'd pull)]
    :in    ['id]
    :where [['d :data/key 'id]]}
   data-k))

(defn operations
  [db amos-id pull]
  (mapv
   first
   (rxt/q
    db
    {:find  [(list 'pull 'o pull)]
     :in    ['id]
     :where [['a :amos/id 'id]
             ['a :amos/operations 'o]]}
    amos-id)))

(defn operation
  [db operation-name pull]
  (->> (rxt/q
        db
        {:find  [(list 'pull 'o pull)]
         :in    ['opname]
         :where [['o :operation/info 'i]
                 ['(get i :info/name) 'n]
                 ['(= opname n)]]}
        operation-name)
       (map first)
       first))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

(defn registry
  [db amos-id]
  (let [definitions
        (map
         first
         (rxt/q
          db
          '{:find  [(pull d [:data/key :data/schema])]
            :in    [id]
            :where [[e :amos/id id]
                    [e :amos/definitions d]]}
          amos-id))
        definition-registry
        (reduce
         (fn [acc {:data/keys [key schema]}] (assoc acc key schema))
         {}
         definitions)]
    (usc/make-registry definition-registry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Old API in need of conversion

;;;;
;; Fix these helpers

;; Should return the schema for the operations
(defn find-parameter-spec
  [{:amos/keys [id] :as _old-amos} op]
  ;; TODO - need to provide an aggregation of schemas
  (log/infof "Find parameter spec for: %s" op)
  (let [schemas     (->>
                     (operation 'db op '[:operation/parameters])
                 :operation/parameters
                 (map :data/schema))
        aggregation (cond
                      (= :ref (ffirst schemas)) (first schemas)
                      (= :map (ffirst schemas)) (reduce (fn [acc s]
                                                          (mu/merge acc s))
                                                        [:map]
                                                        schemas)
                      :else                     (first schemas))]
    (log/infof "Found schema: %s " (into [] schemas))
    aggregation)
  
  ;;(throw (Exception. "Not implemented"))
  )

(defn find-in-spec
  [amos op spec-k]
  (log/error (str "find-in-spec called " op " : " spec-k))
  (let [schemas (->>
                 (operation 'db op '[:operation/responses])
                 :operation/responses
                 (map :data/schema))]
    (log/infof "Found schema: %s " (into [] schemas))
    (cond 
      (= :ref (ffirst schemas)) (first schemas)
      (= :map (ffirst schemas)) (reduce (fn [acc s] (mu/merge acc s)) [:map] schemas)
      :else (first schemas)))
  ;;(throw (Exception. "Not implemented"))
  )

(comment
  
  )

(defn merge-amos-data-specs-into-schemas
  [{:amos/keys [id] :as _old-amos}]
  (:registry
   (registry 'db id))
  ;;(throw (Exception. "Not implemented"))
  )

(defn get-amos-operations
  [{:amos/keys [id] :as _old-amos}]
  ;;(log/info "get-amos-operations" _old-amos)
  (log/infof "get-amos-operations for AMOS id %s" id)
  (let [operation-names (->>
                         (operations 'db id '[:operation/info])
                         (map first)
                         (map :operation/info)
                         (map :info/name))]
    (log/infof "Found AMOS operations: %s" (into [] operation-names))
    operation-names

        ;;addProduct experiment
    '("deleteProductByName"
      "addProduct"
      "getProductByName"
      "getAllProducts")
    )
  ;;(throw (Exception. "Not implemented"))
  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Validation

(defn validate
  "Validate the given value with the given schema. A registry of the definitions
  of the provided AMOS-id is used in the validation."
  [db amos-id schema value]
  (let [definitions
        (map
         first
         (rxt/q
          db
          '{:find  [(pull d [:data/key :data/schema])]
            :in    [id]
            :where [[e :amos/id id]
                    [e :amos/definitions d]]}
          amos-id))
        definition-registry
        (reduce
         (fn [acc {:data/keys [key schema]}] (assoc acc key schema))
         definitions)]
    (usc/validate schema value {:registry definition-registry})))

(defn humanize-validation
  [db amos-id schema value]
  (let [definitions
        (map
         first
         (rxt/q
          db
          '{:find  [(pull d [:data/key :data/schema])]
            :in    [id]
            :where [[e :amos/id id]
                    [e :amos/definitions d]]}
          amos-id))
        definition-registry
        (reduce
         (fn [acc {:data/keys [key schema]}] (assoc acc key schema))
         definitions)]
    (usc/humanize schema value definition-registry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Relations

(defn create-amos-operation->index
  [db {:amos/keys [id] :as _old-amos} my-node-include node-include cost-limit]
  (let [operation-nodes (->>
                         (ras/nodes-of-type
                          db :operation [:schema.node/id :schema.node/name])
                         (map first)
                         ;;(map :schema.node/id)
                         )
        graph           (ras/make-graph db)
        ]
    ;;operation-nodes
    (reduce
     (fn [acc op-node]
       (let [components (ras/node-components db (:schema.node/id op-node)
                                             '[*])
             parameters (first (filter #(clojure.string/includes?
                                         (:schema.node/id %) my-node-include)
                                       components))
             parameter-name (:schema.node/id parameters) ;;(:schema.node/id op-node)
             related-responses
             (->> (ualg/bf-traverse graph parameter-name)
                  (filter #(clojure.string/includes? % node-include)
                          ;;(clojure.string/includes? % "parameter")
                          )
                  (map #(ualg/shortest-path graph
                                            {:start-node parameter-name
                                             :end-node   %})))
             
             by-cost (sort-by :cost (filter #(< (:cost %) cost-limit) related-responses))
             related-operations
             (map #(->> (ras/query-component-references db (:end %))
                        ffirst
                        :schema.node/_components
                        (map :schema.node/name))
                  by-cost)]
         ;;parameters
         (assoc acc (:schema.node/name op-node)
                (filterv #(not= (:schema.node/name op-node) %)
                         (flatten related-operations)))
         ))
     {}
     operation-nodes)))

(defn create-parameter->parameter-index
  [db {:amos/keys [id] :as old-amos} cost-limit]
  (let [operation-nodes (->>
                         (ras/nodes-of-type
                          db :operation [:schema.node/id :schema.node/name])
                         (map first)
                         ;;(map :schema.node/id)
                         )
        graph           (ras/make-graph db)
        ]
    (reduce
     (fn [acc op-node]
       (let [components (ras/node-components db (:schema.node/id op-node)
                                             '[*])
             parameters (first (filter #(clojure.string/includes?
                                         (:schema.node/id %) "parameter")
                                       components))
             parameter-name (:schema.node/id parameters) ;;(:schema.node/id op-node)
             related-responses
             (->> (ualg/bf-traverse graph parameter-name)
                  (filter #(clojure.string/includes? % "parameter")
                          ;;(clojure.string/includes? % "parameter")
                          )
                  (map #(ualg/shortest-path graph
                                            {:start-node parameter-name
                                             :end-node   %})))
             
             by-cost (sort-by :cost (filter #(< (:cost %) cost-limit) related-responses))]
         ;;parameters
         by-cost))
     {}
     operation-nodes)))

(defn create-amos-operation->reponse-index
  "Given an amos, returns an index of operation -> vector of related operations"
  [db {:amos/keys [id] :as old-amos} cost-limit]
  (create-amos-operation->index db old-amos "parameter" "response" cost-limit))

(defn create-amos-operation->parameter-index
  "Given an amos, returns an index of operation -> vector of related operations"
  [db {:amos/keys [id] :as old-amos} cost-limit]
  (create-amos-operation->index db old-amos "parameter" "parameter" cost-limit))

(defn create-amos-operation->response-parameter-index
  "Given an amos, returns an index of operation -> vector of related operations"
  [db {:amos/keys [id] :as old-amos} cost-limit]
  (create-amos-operation->index db old-amos "response" "parameter" cost-limit))

