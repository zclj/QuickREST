(ns quickrest.alpha.resources.exploration.result
  (:require [malli.util :as mu]
            [quickrest.alpha.resources.xtdb :as rxt]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Resource to store objective and method results
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exploration results

;;;;
;; Schemas

(def TimeUnit [:enum :msec :s :m :h])

(def Time
  [:map
   [:time :double]
   [:unit TimeUnit]])

(def Metrics
  [:map
   [:metric/time Time]])

(def Session
  [:map
   [:session/id uuid?]
   [:session/metrics Metrics]])

(def Property
  [:map
   [:property/key :keyword]])

(def OperationInfo
  [:map
   [:info/name :string]])

(def Operation
  [:map
   [:operation/id uuid?]
   [:operation/info OperationInfo]])

(def Context
  [:map
   [:explored/operation Operation]])

(def Generation
  [:map
   [:generation/value
    [:map-of {:gen/schema [:map
                           ["foo" {:optional true} :string]
                           ["bar" {:optional true} :int]]}
     :string :any]]
   [:generation/id uuid?]])

(def Parameter
  [:map
   [:parameter/name :string]
   [:parameter/generation Generation]
   [:parameter/value
    [:map-of {:gen/schema [:map
                           ["foo" {:optional true} :string]
                           ["bar" {:optional true} :int]]}
     :string :any]]])

(def OperationParameters
  [:map
   [:operation/parameters
    [:vector {:gen/min 0 :gen/max 3} Parameter]]])

(def OperationExample
  (mu/merge
   Operation
   OperationParameters))

(def Example
  [:vector OperationExample])

(def Result
  [:map
   [:exploration/session Session]
   [:exploration/example Example]
   [:exploration/property Property]
   [:exploration/context Context]
   [:exploration/id uuid?]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Construction

(defn make-session-time-metric
  {:malli/schema [:=> [:cat :double TimeUnit] Metrics]}
  [duration unit]
  {:metric/time
   {:time duration
    :unit unit}})

(defn make-session
  {:malli/schema [:=> [:cat Metrics] Session]}
  [metrics]
  {:session/id      (random-uuid)
   :session/metrics metrics})

(defn make-result
  {:malli/schema [:=> [:cat Session Example Property Context] Result]}
  [session example property context]
  (-> {:exploration/session  session
       :exploration/example  example
       :exploration/property property
       :exploration/context  context
       :exploration/id       (random-uuid)}
      (rxt/->entity :exploration/id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transactions

;; TODO - make entities of the different parts of the result
(defn ->tx
  {:malli/schema [:=> [:cat [:vector Result]] :any]}
  [results]
  (rxt/->txs results))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query

(defn results
  [db pull]
  (->>
   (rxt/q
    db
    {:find  [(list 'pull 'e pull)]
     :where [['e :exploration/id]]})
   (map first)))

(defn result
  [db result-id pull]
  (->>
   (rxt/q
    db
    {:find  [(list 'pull 'e pull)]
     :in    ['?id]
     :where [['e :exploration/id '?id]]}
    result-id)
   (map first)
   first))
