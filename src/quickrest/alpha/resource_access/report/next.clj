(ns quickrest.alpha.resource-access.report.next
  (:require [quickrest.alpha.resources.exploration.result :as rer]))

(defn pretty-parameter
  [param]
  {:name  (:parameter/name param)
   :value (:parameter/value param)})

(defn pretty-parameters
  [params]
  (mapv pretty-parameter params))

(defn pretty-operation
  [operation]
  (let [params (:operation/parameters operation)]
    {:operation  (get-in operation [:operation/info :info/name])
     :parameters (pretty-parameters params)}))

(defn str-parameter
  [param]
  ;;(println param)
  (str (:parameter/name param) " = " (first (vals (:parameter/value param)))))

(defn str-parameters
  [params]
  (clojure.string/join ", " (mapv str-parameter params)))

(defn pretty-example
  [example]
  (let [explored-op (-> (get-in example [:exploration/context :explored/operation])
                        (get-in [:operation/info :info/name]))
        prop-k      (:property/key (:exploration/property example))]
    (if (= (:operation/id (first (:exploration/example example)))
             #uuid "deadbeef-dead-beef-dead-beef00000075")
      {explored-op "No example found"}
      (cond
        (= prop-k :property/response-equality-property)
        {explored-op (str "fullfill response equality with "
                          (str-parameters
                           (:operation/parameters (first (:exploration/example example)))))
         }
        :else
        {explored-op
         {:operation-sequence
          (mapv pretty-operation (:exploration/example example))}}))))

(defn report
  [db id]
  (let [exploration (rer/result db id [:exploration/example
                                       :exploration/session
                                       :exploration/context
                                       :exploration/property])]
    (pretty-example exploration)))
