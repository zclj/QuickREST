(ns quickrest.alpha.managers.objective.manager
  (:require [quickrest.alpha.engines.method.exploration-v2 :as eme]
            [clojure.tools.logging :as log]
            [quickrest.alpha.resources.amos :as ram]
            [quickrest.alpha.resource-access.schema :as ras]
            [quickrest.alpha.engines.method.exploration-v2.meta-operation-generators :as mog]
            [quickrest.alpha.resources.xtdb :as rxt]
            [quickrest.alpha.resources.exploration.result :as rer]
            [quickrest.alpha.resource-access.report.pretty :as repretty]
            [quickrest.alpha.engines.method.fuzz :as emf]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Experimental reporter (move this)

(defn pretty-report
  [{:exploration/keys [example] :as exploration-result}]
  (let [pretty-examples
        (mapv
         (fn [example-op]
           (let [clean-example
                 (dissoc example-op :operation/responses :schema/nodes :xt/id)
                 clean-params (mapv #(select-keys % [:parameter/name :http/in :parameter/generation :parameter/value]) (:operation/parameters example-op))]
             (assoc clean-example :operation/parameters clean-params)))
         example)
        clean-context (update-in exploration-result
                                 [:exploration/context :query/operation]
                                 (fn [op]
                                   (select-keys op [:operation/id :operation/info])))]
    (-> clean-context
        (assoc :exploration/example pretty-examples)))
  )

(defn mini-report
  [exploration-result]
  (mapv :info/name
        (mapv :operation/info
              (mapcat :exploration/example
                      (mapv pretty-report exploration-result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils to move

(defn filter-query-op
  [{:keys [:operation/id] :as _query-op} responses]
  (filterv (fn [rsp]
             (= id (:operation/id rsp)))
           responses))

(defn operation-name
  [op]
  (get-in op [:operation/info :info/name]))

(defn operation-names
  [ops]
  (into [] (mapv operation-name ops)))

(defn domain->server-meta
  [{:keys [domain/meta-data]}]
  (cond-> {}
    (get meta-data "basePath") (assoc :http/base-path (get meta-data "basePath"))
    (get meta-data "consumes") (assoc :http/consumes (get meta-data "consumes"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exploration

(defn explore
  [node amos-id exploration-fn exploration-name]
  (let [domain            (ram/domain (rxt/db node) amos-id)
        server-meta       (domain->server-meta domain)
        raw-result        (exploration-fn server-meta)
        result-tx         (rxt/submit-tx-await node (rer/->tx raw-result))
        pretty-report     (into
                           []
                           (sort-by
                            #(first (keys %))
                            (mapv #(repretty/report (rxt/db node) (:exploration/id %))
                                  raw-result)))
        summary           (repretty/summary pretty-report)
        pretty-report-str (str (with-out-str (clojure.pprint/pprint pretty-report))
                               summary
                               "\n")]
    (log/debug "Result transaction" result-tx)
    (.mkdir (java.io.File. "./out"))
    (spit (format "./out/%s.edn" exploration-name) raw-result)
    (spit (format "./out/%s-pretty.edn" exploration-name) pretty-report-str)
    (conj pretty-report summary)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default exploration context

(defn filter-unsupported-operations
  [ops]
  (filterv
   #(not= (:info/produces (:operation/info %)) ["application/octet-stream"])
   ops))

(defn fuzz-operation
  [node amos-id scheme host port nrof-tests]
  (log/debug "Fuzz Operation")
  (explore
   node
   amos-id
   (fn [server-meta]
     (emf/fuzz-operations
      (merge
       emf/default-context
       {:operations-fn
        (fn [_]
          (log/debug "Operations fn")
          (let [raw-ops (ram/operations (rxt/db node) amos-id '[*])
                found-ops
                (filter-unsupported-operations raw-ops)]
            ;; (println (str "Found ops => " raw-ops))
            ;; (println (str "Supported ops => " found-ops))
            found-ops))
        :invoke-operation-fn
        (eme/make-execution-operation
         (merge
          {:http/scheme  scheme
           :request/host host
           :request/port port}
          server-meta))
        :number-of-tests-per-property nrof-tests})))
   "fuzz"))

(defn explore-response-equality-property
  [node amos-id scheme host port nrof-tests]
  (log/info "Explore Response Equality")
  (explore
   node
   amos-id
   (fn [server-meta]
     (eme/response-equality-property
      (merge
       eme/default-context
       {:operations-fn
        (fn [_]
          (log/debug "Operations fn")
          (let [found-ops (filter-unsupported-operations
                           (ram/operations (rxt/db node) amos-id '[*]))]
            found-ops))
        :invoke-operation-fn
        (eme/make-execution-operation
         (merge
          {:http/scheme  scheme
           :request/host host
           :request/port port}
          server-meta))
        :number-of-tests-per-property nrof-tests})))
   "response-equality"))

(defn explore-response-inequality-property
  [node amos-id scheme host port nrof-tests]
  (log/info "Explore Response Inequality")
  (explore
   node
   amos-id
   (fn [server-meta]
     (eme/response-inequality-property
      (merge
       eme/default-context
       {:operations-fn
        (fn [_]
          (log/info "Operations fn")
          (let [found-ops (filter-unsupported-operations
                           (ram/operations (rxt/db node) amos-id '[*]))]
            found-ops))
        :invoke-operation-fn
        (eme/make-execution-operation
         (merge
          {:http/scheme  scheme
           :request/host host
           :request/port port}
          server-meta))
        :number-of-tests-per-property nrof-tests})))
   "response-inequality"))

(defn explore-state-mutation
  [node amos-id scheme host port nrof-tests operation-sequence-min-size operation-sequence-max-size]
  (log/info "Explore State Mutation")
  (explore
   node
   amos-id
   (fn [server-meta]
     (eme/state-mutation-property
      (merge
       eme/default-context
       {:number-of-tests-per-property nrof-tests
        :invoke-operation-fn
        (eme/make-execution-operation
         (merge
          {:http/scheme  scheme
           :request/host host
           :request/port port}
          server-meta))
        :operations-fn
        (fn [_ctx]
          ;;(ram/operations (rxt/db node) amos-id '[* {:schema/nodes [*]}])
          (let [ops
                (filter-unsupported-operations
                 (into
                  (filterv #(= (:http/method %) :post)
                           (ram/operations (rxt/db node) amos-id '[* {:schema/nodes [*]}]))
                  (filterv #(= (:http/method %) :put)
                           (ram/operations (rxt/db node) amos-id '[* {:schema/nodes [*]}]))))]
            ;;(println (str "FOUND OPS => " ops))
            ops))
        :query-operations-fn
        (fn [_ctx]
          (filter-unsupported-operations
           (filterv #(= (:http/method %) :get)
                    (ram/operations (rxt/db node) amos-id '[* {:schema/nodes [*]}]))))
        :reference-lookup-fn
        (fn [operation operation-sequence {:keys [schema/nodes] :as parameter}]
          ;; (log/info "Reference lookup for sequence => " operation-sequence)
          ;; (log/info "Reference lookup for => " parameter)
          (let [graph       (ras/make-graph (rxt/db node)
                                            #{:map/key
                                              :map/entry
                                              :container/map
                                              :responses
                                              :operation})
                root-schema (first (filter :schema.node/root (vals nodes)))
                component   (first (:schema.node/components root-schema))
                relations   (remove #{"keyword"}
                                    (ras/bf-traverse graph component))]
            ;; TODO: keyword schema node type should be added and directed
            (log/debug "Reference for => " (:info/name (:operation/info operation)))
            (log/debug "Reference relations => " relations)
            ;; this is the "any possible relation" should be refined
            relations))
        :post-process-responses-fn
        (fn [_ctx observed-responses]
          (log/info "post-process-responses-fn - STATE Mutation")
          [(first observed-responses) (last observed-responses)])
        :shrink-sequence-fn
        (fn [ctx operation-sequence]
          (mog/shrink-operation-sequence-2
           mog/shrinked-alternatives
           (fn [alternative-sequence]
             (and (mog/valid-reference-operation-sequence? alternative-sequence)
                  (= (:operation/id (first alternative-sequence))
                     (:operation/id (last alternative-sequence))
                     (:operation/id (:session/query-operation ctx)))))
           operation-sequence))})
      operation-sequence-min-size
      operation-sequence-max-size))
   "state-mutation"))

(defn explore-state-identity-with-state-change-observation
  [node amos-id scheme host port nrof-tests operation-sequence-min-size operation-sequence-max-size]
  (log/info "Explore State Identity")
  (explore
   node
   amos-id
   (fn [service-meta]
     (eme/state-identity-with-state-change-observation
         (merge
          eme/default-context
          {:number-of-tests-per-property nrof-tests
           :invoke-operation-fn
           (eme/make-execution-operation
            (merge
             {:http/scheme  scheme
              :request/host host
              :request/port port}
             service-meta))
           :operations-fn
           (fn [_ctx]
             ;;(ram/operations (rxt/db node) amos-id '[* {:schema/nodes [*]}])
             (filter-unsupported-operations
              (into
               (filterv #(= (:http/method %) :post)
                        (ram/operations (rxt/db node) amos-id '[* {:schema/nodes [*]}]))
               (filterv #(= (:http/method %) :delete)
                        (ram/operations (rxt/db node) amos-id '[* {:schema/nodes [*]}]))))
             )
           :query-operations-fn
           (fn [_ctx]
             (filter-unsupported-operations
              (filterv #(= (:http/method %) :get)
                       (ram/operations (rxt/db node) amos-id '[* {:schema/nodes [*]}]))))
           :reference-lookup-fn
           (fn [operation operation-sequence {:keys [schema/nodes] :as parameter}]
             ;; (log/info "Reference lookup for sequence => " operation-sequence)
             ;; (log/info "Reference lookup for => " parameter)
             (let [graph       (ras/make-graph (rxt/db node)
                                               #{:map/key
                                                 :map/entry
                                                 :container/map
                                                 :responses
                                                 :operation})
                   root-schema (first (filter :schema.node/root (vals nodes)))
                   component   (first (:schema.node/components root-schema))
                   relations   (remove #{"keyword"}
                                       (ras/bf-traverse graph component))]
               ;; TODO: keyword schema node type should be added and directed
               (log/debug "Reference for => " (:info/name (:operation/info operation)))
               (log/debug "Reference relations => " relations)
               ;; this is the "any possible relation" should be refined
               relations))
           :post-process-responses-fn
           (fn [{:keys [:session/query-operation] :as _ctx} observed-responses]
             (log/debug "post-process-responses-fn - STATE IDENTITY")
             ;; (log/debug "observed responses " observed-responses)
             ;; (log/debug "query operation " query-operation)
             ;; For this property we should only check the responses of the query-ops
             ;; so remove any other operations
             (let [q-ops
                   (filter-query-op query-operation observed-responses)]
               ;;(log/info "QQ " query-operation)
               ;;(log/info "QQ" (:session/query-operation _ctx))
               ;;(log/debug "QQuery observed query ops " q-ops)
               (log/debug "observed " (into [] (map :operation/info observed-responses)))
               (log/debug "q-ops " (into [] (map :operation/info q-ops)))
               (log/debug "first observed" (first q-ops))
               (log/debug "second observed" (first q-ops))
               q-ops))
           :shrink-sequence-fn
           (fn [ctx operation-sequence]
             ;;(log/info (str "Shrink with query op " (:session/query-operation ctx)))
             (mog/shrink-operation-sequence-2
              mog/shrinked-alternatives
              (fn [alternative-sequence]
                (and (mog/valid-reference-operation-sequence? alternative-sequence)
                     (= (:operation/id (first alternative-sequence))
                        (:operation/id (last alternative-sequence))
                        (:operation/id (:session/query-operation ctx)))))
              operation-sequence))})
         operation-sequence-min-size
         operation-sequence-max-size))
   "state-identity"))

(defn explore-properties
  [node amos-id {:keys [http/scheme request/host request/port nrof-tests min-seq-size max-seq-size]
                 :as _options}
   properties]
  (log/debug (str "Exploration with AMOS " amos-id))
  (log/debug _options)
  (log/debug (str "Explore properties: " properties))
  (cond
    (= :fuzz (first properties))
    (fuzz-operation node amos-id scheme host port nrof-tests)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; MP-R-1 - Response equality
    (= :response-equality (first properties))
    (explore-response-equality-property node amos-id scheme host port nrof-tests)
    (= :response-inequality (first properties))
    (explore-response-inequality-property node amos-id scheme host port nrof-tests)
    (= :state-mutation (first properties))
    (explore-state-mutation node amos-id scheme host port nrof-tests min-seq-size max-seq-size)
    (= :state-identity (first properties))
    (explore-state-identity-with-state-change-observation
     node amos-id scheme host port nrof-tests min-seq-size max-seq-size)
    :else {:execution/error {:error :non-supported-property
                             :message (str "Unknown behavior " (first properties))}}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test execution

(defn summarize-test-results
  [raw-results]
  (let [executed-tests (filterv #(not= :skipped (:test/result %)) raw-results)
        failed         (filterv #(= :failed (:test/result %)) raw-results)]
    {:tests-executed executed-tests
     :summary        {:tests/run (count executed-tests) :tests/failed (count failed)}
     ;;(str "Tests run: " (count executed-tests) ", Failures: " (count failed))
     }))

(defn execute-examples
  ;; TODO: make this better.. read from a resource access etc.
  [file-name {:keys [http/scheme request/host request/port] :as _options}]
  (let [exploration-sessions (clojure.edn/read-string (slurp file-name))
        test-result          (doall (map
                                     (fn [exploration-session]
                                       ;;(println exploration-session)
                                       (eme/execute-exploration-example
                                        (eme/make-execution-operation
                                         {:http/scheme  scheme
                                          :request/host host
                                          :request/port port})
                                        exploration-session))
                                     exploration-sessions))
        summary              (summarize-test-results test-result)]
    summary))

(comment
  (execute-examples "./out/results.edn" {:http/scheme  :http
                                         :request/host "rest-vm"
                                         :request/port 8080})
  )
