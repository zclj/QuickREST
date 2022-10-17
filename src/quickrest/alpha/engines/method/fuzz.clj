(ns quickrest.alpha.engines.method.fuzz
  (:require [quickrest.alpha.engines.method.result.exploration :as emres]
            [quickrest.alpha.engines.method.exploration-v2.meta-properties :as mp]
            [clojure.tools.logging :as log]
            [quickrest.alpha.engines.method.exploration-v2.invocation :as inv]
            [clojure.test.check :as tc]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common with exploration

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
             (= 500 (get-in r [:invocation/response :response/payload :status]))))))
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

(defn fuzz-operations
  [{:keys [operations-fn] :as ctx}]
  (let [operations-to-check (operations-fn ctx)]
    (mapv
     (fn [operation]
       (emres/with-exploration-result
         #(check-property ctx (fn [ctx] (mp/fuzz-operation ctx operation)))
         :property/fuzz-operations
         operation))
     operations-to-check)))
