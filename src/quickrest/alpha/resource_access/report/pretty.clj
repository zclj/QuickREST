(ns quickrest.alpha.resource-access.report.pretty
  (:require [quickrest.alpha.resources.exploration.result :as rer]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Given an exploration result, produces a pretty report, target at REPL or CLI
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pretty-parameter
  [param]
  {:name  (:parameter/name param)
   :value (:parameter/value param)}
  ;; TODO - revisit when symbolic references are in place
  ;; (cond-> {:name       (:parameter/name param)
  ;;          :value      (:parameter/value param)}
  ;;   (:parameter/value param) (assoc :generation (:parameter/generation param)))
  )

(defn pretty-parameters
  [params]
  (mapv pretty-parameter params))

(defn pretty-operation
  [operation]
  (let [params (:operation/parameters operation)]
    {:operation  (get-in operation [:operation/info :info/name])
     :parameters (pretty-parameters params)}))

(defn pretty-example
  [example]
  (let [explored-op (-> (get-in example [:exploration/context :explored/operation])
                        (get-in [:operation/info :info/name]))]
    {explored-op
     {:operation-sequence
      (mapv pretty-operation (:exploration/example example))}}))

(defn report
  [db id]
  (let [exploration (rer/result db id [:exploration/example
                                       :exploration/session
                                       :exploration/context])]
    ;;[exploration]
    (pretty-example exploration)))

(defn summary
  [ops]
  (let [groups
        (group-by
         (fn [op]
           (= "No example found"
              (:operation (first (:operation-sequence (first (vals op)))))))
         ops)
        successes (second (first groups))
        failures  (second (second groups))]
    {:examples         (count successes)
     :no-examples      (count failures)
     :total-operations (count ops)}))
