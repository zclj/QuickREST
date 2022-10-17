(ns quickrest.alpha.engines.method.exploration-v2.meta-properties
  (:require [clojure.test.check.properties :as prop]
            [quickrest.alpha.engines.method.exploration-v2.meta-operation-generators
             :as mog]
            [quickrest.alpha.engines.method.exploration-v2.invocation :as inv]
            [clojure.test.check.generators :as gen]
            [clojure.tools.logging :as log]
            [malli.generator :as mg]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

(defn make-execution-result
  [operation response]
  {:execution/operation operation
   :execution/response  response})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Meta Properties

(defn basic-property
  "A basic property is run without consideration for changing state.
  The given sequence generator will be used to generate a sequence to execute.
  The generated sequence will then be executed, with the given invokation fn, and the results will be checked, with the given check predicate.

  The pre-execution-fn will be called before any operations in the execution sequence are invoked. This allow a client to do any preparations needed before a new sequence is executed."
  [{:keys [operation-sequence-generator
           pre-sequence-execution-fn
           pre-operation-exection-fn
           post-operation-execution-fn
           invoke-operation-fn
           process-response-fn
           post-process-responses-fn
           pre-check-fn
           check-predicate-fn
           post-check-fn]
    :as   ctx}]
  (prop/for-all
    ;; Generate the sequence
    [execution-sequence (operation-sequence-generator)]
    (let [;; inform the client that we are preparing to run the sequence
          _               (pre-sequence-execution-fn ctx execution-sequence)
          observed-responses
          (doall
           (mapv
            (fn [generated-operation]
              (pre-operation-exection-fn ctx generated-operation)
              (let [execution-response (invoke-operation-fn ctx generated-operation)
                    processed-response
                    (process-response-fn ctx generated-operation execution-response)
                    _                  (post-operation-execution-fn
                                        ctx generated-operation processed-response)]
                processed-response))
            execution-sequence))
          ;; allow the client to process the responses before 'check'
          final-responses (post-process-responses-fn ctx observed-responses)
          ;; inform the client that we are about to 'check'
          _               (pre-check-fn
                           ctx execution-sequence observed-responses final-responses)
          ;; make the check of the responses
          check-result    (check-predicate-fn
                           ctx execution-sequence observed-responses final-responses)
          ;; inform the client of the check result
          _
          (post-check-fn
           ctx execution-sequence observed-responses final-responses check-result)]
      check-result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties

(defn check-response-equality
  {:malli/schema [:=> [:cat inv/InvocationResults] :boolean]}
  [invocation-results]
  ;; we should only consider operation key and parameter with the response
  (let [result-wo-ids (map #(dissoc % :response/id)
                           (map :invocation/response invocation-results))]
    (not (apply = result-wo-ids))))

(defn check-response-inequality
  {:malli/schema [:=> [:cat inv/InvocationResults] :boolean]}
  [invocation-results]
  ;; we should only consider operation key and parameter with the response
  (let [result-wo-ids (map #(dissoc % :response/id)
                           (map :invocation/response invocation-results))]
    (apply = result-wo-ids)))

(defn check-state-mutation
  {:malli/schema [:=> [:cat inv/InvocationResults] :boolean]}
  [invocation-results]
  ;; we should only consider operation key and parameter with the response
  (let [result-wo-ids (map #(dissoc % :response/id)
                           (map :invocation/response invocation-results))]
    (apply = result-wo-ids)))

(defn dissoc-response-ids
  [invocation-results]
  (map #(dissoc % :response/id) (map :invocation/response invocation-results)))

(defn check-state-identity-with-observation
  {:malli/schema [:=> [:cat inv/InvocationResults] :boolean]}
  [invocation-results]
  (let [result-wo-ids (dissoc-response-ids invocation-results)
        first-query-r (first result-wo-ids)
        last-query-r  (last result-wo-ids)
        observations  (butlast (rest result-wo-ids))]
    ;; (log/debug "fqop " first-query-r)
    ;; (log/debug "lqop " last-query-r)
    ;; (log/debug "obs" observations)
    ;; first and last should be equal, i.e., the identity
    (if (= first-query-r last-query-r)
      ;; in addition, the observations should not be equal (remember inverse check..)
      (apply = result-wo-ids)
      true)
    ))

(comment
  (check-state-identity-with-observation
   [:a :b :c :a])
  )

(defn response-equality
  "Is the observed response for an execution sequence of the same operation with the same parameters equal?"
  [ctx operation]
  (basic-property
   (merge ctx
          {:operation-sequence-generator
           (fn [] (mog/identity-meta-operation-sequence ctx operation 2))
           :check-predicate-fn
           (fn [ctx execution-sequence observed-responses final-responses]
             (if (seq final-responses)
               (check-response-equality final-responses)
               ;; pass on no results
               true
               ))})))

(defn response-inequality
  "Is the observed response for an execution sequence of the same operation with the same parameters NOT equal?"
  [ctx operation]
  (basic-property
   (merge ctx
          {:operation-sequence-generator
           (fn [] (mog/identity-meta-operation-sequence ctx operation 2))
           :check-predicate-fn
           (fn [ctx execution-sequence observed-responses final-responses]
             (if (seq final-responses)
               (check-response-inequality final-responses)
               ;; pass on no results
               true
               ))})))

(defn state-mutation
  "Is the observed response from the first/last different?"
  [ctx query-operation operations reference-lookup-fn shrink-sequence-fn min-size max-size]
  (basic-property
   (merge ctx
          {:operation-sequence-generator
           (fn []
             (mog/state-mutation-sequence
              ctx query-operation operations reference-lookup-fn shrink-sequence-fn min-size max-size))
           :check-predicate-fn
           (fn [ctx execution-sequence observed-responses final-responses]
             (if (seq final-responses)
               (check-state-mutation final-responses)
               ;; pass on no results
               true
               ))})))

(defn state-identity-with-observation
  "Is the observed response from the first/last the same? In addition, an 'observation'
  , meaning that the response from the query-op should have changed in the sequence"
  [ctx query-operation operations reference-lookup-fn shrink-sequence-fn min-size max-size]
  (basic-property
   (merge ctx
          {:operation-sequence-generator
           (fn []
             (mog/state-identity-with-observation-sequence
              ctx query-operation operations reference-lookup-fn shrink-sequence-fn
              min-size max-size))
           :check-predicate-fn
           (fn [ctx execution-sequence observed-responses final-responses]
             (if (and (seq final-responses) (> (count final-responses) 2))
               (check-state-identity-with-observation final-responses)
               ;; pass on no results
               (do
                 (log/debug "Pass with no result to check")
                 true)
               ))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fuzz properties

(defn check-fuzz-operations
  [responses]
  (not (seq responses)))

(defn fuzz-operation
  [ctx operation]
  (basic-property
   (merge ctx
          {:operation-sequence-generator
           (fn [] (mog/identity-meta-operation-sequence ctx operation 1))
           :check-predicate-fn
           (fn [ctx execution-sequence observed-responses final-responses]
             ;; any operation not acheiving the fuzz target should have been filtered
             (check-fuzz-operations final-responses))})))
