(ns quickrest.alpha.resource-access.schema
  (:require [quickrest.alpha.resources.xtdb :as rxt]
            [ubergraph.core :as uber]
            [ubergraph.alg :as ualg]))

(defn pprint
  [graph]
  (uber/pprint graph))

(defn pprint-path
  [path]
  (ualg/pprint-path path))

(defn count-nodes
  [db]
  (-> (rxt/q
       db
       '{:find  [(count e)]
         :where [[e :schema.node/id]]})
      ffirst))

(defn query-component-references
  ([db node-id]
   (query-component-references db node-id '...))
  ([db node-id n]
   (rxt/q
    db
    {:find  [(list 'pull 'e [:schema.node/name :schema.node/type :schema.node/id
                             {:schema.node/_components n}])]
      :where [['e :schema.node/id 'id]]
      :in ['id]}
    node-id)))

(defn query-components
  ([db node-id]
   (query-components db node-id '...))
  ([db node-id n]
   (rxt/q
    db
    {:find  [(list 'pull 'e [:schema.node/name :schema.node/type :schema.node/id
                             {:schema.node/components n}])]
      :where [['e :schema.node/id 'id]]
      :in ['id]}
    node-id)))

(defn has-component
  [db component-id pull]
  (rxt/q
    db
    {:find  [(list 'pull 'e pull)]
      :where [['e :schema.node/components 'id]]
      :in ['id]}
    component-id))

(defn node-components
  [db node-id pull]
  (->>
   (rxt/q
    db
    {:find  [(list 'pull 'c pull)]
     :in    ['?id]
     :where [['e :schema.node/id '?id]
             ['e :schema.node/components 'c]]}
    node-id)
   (map first)))

(defn nodes
  [db pull]
  (rxt/q
   db
   {:find  [(list 'pull 'e pull)]
    :where [['e :schema.node/id]]}))

(defn node
  [db node-id pull]
  (->>
   (rxt/q
    db
    {:find  [(list 'pull 'e pull)]
     :in    ['?id]
     :where [['e :schema.node/id '?id]]}
    node-id)
   (map first)
   first))

(defn nodes-of-type
  [db t pull]
  (->>
   (rxt/q
    db
    {:find  [(list 'pull 'e pull)]
     :in    ['?t]
     :where [['e :schema.node/id]
             ['e :schema.node/type '?t]]}
    t)
   (mapv first)))

(defn operations
  [db pull]
  (rxt/q
   db
   {:find  [(list 'pull 'e pull)]
    :where [['e :schema.node/id]
            ['e :schema.node/type :operation]]}))

(defn neighbours
  "Returns the node-ids of the given nodes components"
  [db node-id]
  (->> (rxt/q
        db
        '{:find  [?c]
          :in    [?e]
          :where [[?e :schema.node/components ?c]]}
        node-id)
       (map first)))

;; Is this similar to the amos node collection?
(defn neighbours-graph-nodes
  [db]
  (let [nds (nodes db [:schema.node/id :schema.node/components])]
    (apply
     merge
     (map (fn [{:schema.node/keys [id components]}]
            {id components})
          (filter (fn [n]
                    (not (clojure.string/includes? n "operation")))
                  (map first nds))))))

;; direct - map/entry
(defn make-graph
  "Creates a graph of the schema nodes in the db. All edges will be bi-directional
   if not included in the `directed-types`."
  ([db]
   (make-graph db #{:operation}))
  ([db directed-types]
   (let [nds (nodes db [:schema.node/id :schema.node/components :schema.node/type])]
     (reduce
      (fn [acc {:schema.node/keys [id components type] :as _node}]
        (let [node-ids   (mapv (fn [c] [id c]) components)
              ;;_ (println (str "Type " type))
              add-node-f (if (directed-types type)
                           uber/add-directed-edges
                           uber/add-edges)]
          (apply add-node-f acc node-ids)))
      (uber/graph)
      (map first nds)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graph processing

(defn bf-traverse
  [graph node-id]
  (ualg/bf-traverse graph node-id))

(defn shortest-path
  [graph start-node-id end-node-id]
  (ualg/shortest-path
   graph
   {:start-node start-node-id
    :end-node   end-node-id}))

