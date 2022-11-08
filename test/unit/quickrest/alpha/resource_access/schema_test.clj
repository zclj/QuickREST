(ns unit.quickrest.alpha.resource-access.schema-test
  (:require [quickrest.alpha.repl :as repl]
            [quickrest.alpha.resource-access.schema :as sut]))

(comment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Make a graph and provide the different types of query needed

  (-> (sut/make-graph (repl/db) #{:map/entry :operation :parameters :responses})
      (sut/pprint))

;;;;
  ;; Queries we need
  ;; - param to param schema
  ;; - operation to operation
  ;; - param to response schema

  (sut/operations (repl/db) '[:schema.node/id])

  (sut/neighbours (repl/db) "operation/deleteProductByName")

  (sut/neighbours (repl/db) "operation/addConfiguration")
  (sut/query-components (repl/db) "parameters/addConfiguration")
  (sut/query-components (repl/db) "parameters/addConfiguration" 2)

  (sut/neighbours (repl/db) "operation/getAllProducts")
  (sut/node (repl/db) "responses/getAllProducts" '[*])
  (sut/query-components (repl/db) "responses/getAllProducts")

  (sut/neighbours (repl/db) "operation/addProduct")
  (sut/node (repl/db) "operation/addProduct" '[*])
  (sut/node (repl/db) "parameters/addProduct" '[*])
  ;; this is the 'root'
  (sut/node (repl/db) #uuid "2e1f8068-9692-4d02-8187-5f42dd656bd6" '[*])

  (sut/neighbours-graph-nodes (repl/db)))

(comment
;;;;
  ;; Relation example

  (let [graph (sut/make-graph (repl/db) #{:map/key
                                          :map/entry
                                          :container/map
                                          :responses
                                          :parameters
                                          :operation})
        root-schema (sut/node (repl/db)
                              ;;#uuid "2e1f8068-9692-4d02-8187-5f42dd656bd6"
                              ;;"map.entry/configurationName::string"
                              #uuid "4f7f0a32-8c29-4761-a997-fed81a970076"
                              '[*])
        component   (first (:schema.node/components root-schema))
        relations   (remove #{"keyword"}
                            (sut/bf-traverse graph component))]
    ;; TODO: keyword schema node type should be added and directed
    
    ;; this is the "any possible relation" should be refined
    [root-schema component relations]
    
    ))

(comment
;;;;
  ;; Does anyone else have my exact structure?

  ;; query for nodes that has me as component - these will have params/responses
  ;;   with the same shape as me
  (sut/has-component (repl/db) "map.entry/productName::string" '[*])
  (sut/has-component (repl/db) "map.entry/configurationName::string" '[*])

  ;; query for nodes that are related to my value type - these can be used to
  ;;  get the value for my parameter
  (sut/node (repl/db) "map.entry/productName::string" '[*])

  (-> (sut/query-components (repl/db) "map.entry/productName::string")
      
      ))

(comment
;;;;
  ;; What operations are related to me

  ;; should all parameters match?

  (sut/node (repl/db) "operation/getAllProducts" '[*])

  (def operation-nodes
    (sut/nodes-of-type (repl/db) :operation '[*]))

  (map
   (fn [op]
     (sut/shortest-path
      (sut/make-graph (repl/db) #{;; :map/key
                                  ;; :map/entry
                                  ;; :container/map
                                  ;;:responses
                                  :parameters
                                  ;;:operation
                                  })
      "operation/getAllProducts"
      (:schema.node/id op)))
   operation-nodes)

  (->
   (sut/shortest-path
    (sut/make-graph (repl/db) #{;; :map/key
                                ;; :map/entry
                                ;; :container/map
                                ;;:responses
                                :parameters
                                ;; :operation
                                })
    "operation/getAllProducts"
    ;;"operation/addFeatureToConfiguration"
    "operation/getConfigurationActivedFeatures"
    )
   (sut/pprint-path)))

