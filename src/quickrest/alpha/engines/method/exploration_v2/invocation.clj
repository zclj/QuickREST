(ns quickrest.alpha.engines.method.exploration-v2.invocation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invokation

(def Passed :passed)

(def Failed :failed)

(def Result
  [:map
   [:result/status [:enum Failed Passed]]])

(def Response
  [:map
   [:response/id uuid?]
   [:response/payload :any]])

(def InvocationResult
  [:map
   [:invocation/operation-id uuid?]
   [:invocation/id uuid?]
   [:invocation/result Result]
   [:invocation/response Response]])

(def InvocationResults
  [:vector InvocationResult])

(defn make-result
  [result]
  {:result/status result})

(defn make-response
  [payload]
  {:response/id      (random-uuid)
   :response/payload payload})

(defn make-invocation-result
  [operation-id result response]
  {:invocation/operation-id operation-id
   :invocation/id           (random-uuid)
   :invocation/result       result
   :invocation/response     response})

(defn with-invocation-result
  [m result]
  (merge m result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filter

(defn filter-invocations-by-status
  "Filters any invocations with a `Failed` status in its result"
  [failed? xs]
  (filterv
   (fn [{:keys [invocation/result]}]
     (failed? (:result/status result)))
   xs))

(defn remove-failed-invocations
  [xs]
  (filter-invocations-by-status #(not= Failed %) xs))

(comment
  (remove-failed-invocations
   [(make-invocation-result (random-uuid)
                            (make-result Passed)
                            (make-response {:foo :bar}))
    (make-invocation-result (random-uuid)
                            (make-result Failed)
                            (make-response {:baz :gizmo}))])
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Status

(defn invocations-status
  [xs]
  (let [total-count   (count xs)
        success-count (count (remove-failed-invocations xs))]
    {:invocations/count        total-count
     :invocations/failed-count (- total-count success-count)}))
