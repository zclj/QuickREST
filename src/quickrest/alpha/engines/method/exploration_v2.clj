(ns quickrest.alpha.engines.method.exploration-v2
  (:require [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.results :as results]
            [clojure.test.check :as tc]
            [quickrest.alpha.engines.method.exploration-v2.meta-operation-generators
             :as mog]
            [quickrest.alpha.engines.method.exploration-v2.meta-properties :as mp]
            [clojure.tools.logging :as log]
            [quickrest.alpha.engines.method.exploration-v2.invocation :as inv]
            [quickrest.alpha.resource-access.domain.rest :as rar]
            [quickrest.alpha.resources.exploration.result :as rer]
            [quickrest.alpha.engines.method.result.exploration :as emres]
            [clj-http.client :as http]
            [cheshire.core :as json]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To refactor

;; TODO: this should live somewhere else
(defn HTTP-status-counts
  [xs]
  {:invocations/http-statuses
   (reduce-kv
    (fn [acc k v]
      (if k
        (assoc acc k (count v))
        acc))
    {}
    (group-by (fn [r] (get-in r [:invocation/response :response/payload :status])) xs))})

;; TODO: this should live somewhere else
(defn make-execution-operation
  [service-info]
  (fn
    [{:keys [amos token reset-fn execution-delay] :as _ctx}
     {:keys [operation/key operation/parameter] :as meta-operation}]
    ;;(log/info "Execute Operation" meta-operation)
    ;; Part of the data contract state that :operation.parameter/value :none
    ;; is provided when there was no parameters to generate values for.
    ;; It differentiates for generating, for example, [], since that might be
    ;; a valid value to generate.
    (let [parameter-map
          (or (apply merge (map :parameter/value (:operation/parameters meta-operation)))
              {})
          invoke-result
          (rar/invoke-url
           (fn [r]
             ;;(log/info (str "Request : ") r)
             (println (str "Request: " r))
             (try
               (let [processed-request
                     (if-let [b (:body r)]
                       (assoc r :body (json/generate-string b))
                       r)
                     response          (http/request
                                        (merge processed-request {:throw-exceptions false}))
                     selected-response (select-keys response [:status :body])]
                 (log/info (str "HTTP Response code : " (:status selected-response)))
                 
                 selected-response)
               (catch Exception e
                 (log/error (str "HTTP Exception " (.getMessage e)))
                 {:execution/failure {:http/exception (.getMessage e)}})))
           (merge meta-operation service-info)
           (:operation/parameters meta-operation)
           parameter-map)
          result-status (if (:execution/failure invoke-result)
                          inv/Failed
                          inv/Passed)]
      ;;(log/info "Parameter value-map : " parameter-map)
      (inv/with-invocation-result
        meta-operation
        (inv/make-invocation-result
         (:operation/id meta-operation)
         (inv/make-result result-status)
         (inv/make-response invoke-result))))))

(defn check-execution-result
  [property-key execution-results]
  ;; @NOTE - the 'check' fns are inverted since they are expressed as the null hypo.
  ;; therefor, invert the result
  (condp = property-key
    :property/response-equality-property
    (not (mp/check-response-equality execution-results))
    :property/state-mutation-property
    (not (mp/check-state-mutation execution-results))
    :property/state-identity-property
    (not (mp/check-state-identity-with-observation execution-results))
    :property/response-inequality-property
    (not (mp/check-response-inequality execution-results))
    :property/fuzz-operations
    (let [statuses (mapv #(get-in % [:invocation/response :response/payload :status]) execution-results)
          error?   (contains? (into #{} statuses) 500)]
      error?)))

(defn execute-exploration-example
  [execution-fn exploration]
  (let [example          (:exploration/example exploration)
        prop             (:exploration/property exploration)
        execution-result (mapv #(execution-fn {} %) example)
        check-result
        (if (= (get-in (first example) [:operation/id])
               #uuid "deadbeef-dead-beef-dead-beef00000075")
          :no-example-to-execute
          (check-execution-result (:property/key prop) execution-result))]
    {:test/result   (cond (= check-result true)  :passed
                          (= check-result false) :failed
                          :else                  :skipped)
     :test/statuses (mapv
                     #(get-in % [:invocation/response :response/payload :status])
                     execution-result)
     :test/name     (str (name (:property/key prop))
                       "/"
                       (:info/name (:operation/info (first example))))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default exploration context

(def default-context
  {:operation-sequence-generator
   (fn []
     (log/warn "Unassigned sequence generator!"))
   :pre-sequence-execution-fn
   (fn [ctx execution-sequence]
     (log/debug "pre-sequence-execution-fn")
     (log/debug "Sequence to execute => "
                (into [] (mapv
                          (fn [op]
                            (get-in op [:operation/info :info/name]))
                          execution-sequence ))))
   :pre-operation-exection-fn
   (fn [ctx generated-operation]
     (log/debug "pre-operation-exection-fn"))
   :post-operation-execution-fn
   (fn [ctx generated-operation execution-response]
     (log/debug "post-operation-execution-fn"))
   :invoke-operation-fn
   (fn [ctx generated-operation]
     (log/debug "invoke-operation-fn"))
   :process-response-fn
   (fn [ctx generated-operation execution-response]
     (log/debug "process-response-fn")
     ;;(log/debug "execution response " execution-response)
     execution-response)
   :post-process-responses-fn
   (fn [ctx observed-responses]
     (log/debug "post-process-responses-fn")
     ;;(log/debug "observed responses " observed-responses)
     (->> (inv/remove-failed-invocations observed-responses)
          (filterv
           (fn [r]
             (not= 500
                   (get-in r [:invocation/response :response/payload :status]))))))
   :pre-check-fn
   (fn [ctx execution-sequence observed-responses final-responses]
     (log/debug "pre-check-fn")
     ;;(log/debug "final responses " final-responses)
     )
   :check-predicate-fn
   (fn [ctx execution-sequence observed-responses final-responses]
     (log/warn "Unassigned check predicate!"))
   :post-check-fn
   (fn [ctx execution-sequence observed-responses final-responses check-result]
     (log/debug "post-check-fn")
     (log/debug "Check result was: " check-result)
     (log/debug "Observed invocations " (inv/invocations-status observed-responses))
     (log/debug "Checked invocations " (inv/invocations-status final-responses))
     (log/debug "Observed HTTP Statuses " (HTTP-status-counts observed-responses)))
   :shrink-sequence-fn
   (fn [ctx operation-sequence]
     (log/warn "Unassigned shrink sequence function!"))})


(defn check-property
  [{:keys [number-of-tests-per-property] :as ctx} property-fn]
  (let [property-check-result
        (tc/quick-check number-of-tests-per-property
                        (property-fn ctx))
        example-sequence
        (if-not (:pass? property-check-result)
          {:shrunk-example
           ;; should there be a first here?
           (vec (first (:smallest (:shrunk property-check-result))))
           :result-data
           (:result-data (:shrunk property-check-result))
           :failing-size (:failing-size
                          property-check-result)
           :num-tests    (:num-tests
                          property-check-result)}
          {:shrunk-example
           [{:operation/id         (java.util.UUID/fromString
                                    "deadbeef-dead-beef-dead-beef00000075")
             :operation/info       {:info/name "No example found"}
             :operation/parameters []}]})]
    example-sequence))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties

;;;;
;; MP-R-1 - Response equality

(defn response-equality-property
  [{:keys [operations-fn] :as ctx}]
  (log/debug "response equality property ctx" ctx)
  (let [operations-to-check (operations-fn ctx)]
    (mapv
     (fn [operation]
       (emres/with-exploration-result
         #(check-property ctx (fn [ctx] (mp/response-equality ctx operation)))
         :property/response-equality-property
         operation))
     operations-to-check)))

;;;;
;; MP-R-2 - Response inequality

(defn response-inequality-property
  [{:keys [operations-fn] :as ctx}]
  (let [operations-to-check (operations-fn ctx)]
    (mapv
     (fn [operation]
       (emres/with-exploration-result
         #(check-property ctx (fn [ctx] (mp/response-inequality ctx operation)))
         :property/response-inequality-property
         operation))
     operations-to-check)))

;;;;
;; MP-S-2 State Mutation

(defn state-mutation-property
  [{:keys [operations-fn query-operations-fn reference-lookup-fn shrink-sequence-fn]
    :as   ctx}
   min-size
   max-size]
  (println (str "Min size " min-size))
  (println (str "Max size " max-size))
  (let [operations-to-check (operations-fn ctx)]
    (mapv
     (fn [query-operation]
       (emres/with-exploration-result
         #(check-property
           ctx
           (fn [ctx]
             (mp/state-mutation ctx query-operation operations-to-check
                                reference-lookup-fn
                                shrink-sequence-fn
                                min-size
                                max-size)))
         :property/state-mutation-property
         query-operation))
     (query-operations-fn ctx))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MP-S-3 State Identity, with state change observation

(defn state-identity-with-state-change-observation
  [{:keys [operations-fn query-operations-fn reference-lookup-fn shrink-sequence-fn]
    :as   ctx}
   min-size
   max-size]
  (let [operations-to-check (operations-fn ctx)]
    (mapv
     (fn [query-operation]
       (emres/with-exploration-result
         #(check-property
           ctx
           (fn [ctx]
             (mp/state-identity-with-observation
              (assoc ctx :session/query-operation query-operation)
              query-operation operations-to-check
              reference-lookup-fn
              shrink-sequence-fn
              min-size
              max-size)))
         :property/state-identity-property
         query-operation))
     (query-operations-fn ctx))
    ;; (mapv
    ;;  (fn [query-operation]
    ;;    (let [start-time (. System (nanoTime))
    ;;          check-result
    ;;          (check-property
    ;;           ctx
    ;;           (fn [ctx]
    ;;             (mp/state-identity-with-observation
    ;;              (assoc ctx :session/query-operation query-operation)
    ;;              query-operation operations-to-check
    ;;              reference-lookup-fn
    ;;              shrink-sequence-fn
    ;;              max-size)))
    ;;          time-taken (/ (double (- (. System (nanoTime)) start-time)) 1000000.0)]
    ;;      {:exploration/session
    ;;       {:session/id        (random-uuid)
    ;;        :session/benchmark {:benchmark/time time-taken
    ;;                            :benchmark/unit :msec}}
    ;;       :exploration/example  (:shrunk-example check-result)
    ;;       :exploration/property :property/state-mutation-property
    ;;       :exploration/context  {:query/operation query-operation}}))
    ;;  (query-operations-fn ctx))
    ))
