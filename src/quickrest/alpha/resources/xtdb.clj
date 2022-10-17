(ns quickrest.alpha.resources.xtdb
  (:require
   [xtdb.api :as xt]
   [integrant.core :as ig]
   [clojure.tools.logging :as log]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API

(defn status
  [node]
  (xt/status node))

(defn db
  [node]
  (xt/db node))

(defn q
  [db query & args]
  (apply xt/q db query args))

(defn entity
  [db eid]
  (xt/entity db eid))

(defn submit-tx
  [node tx]
  (xt/submit-tx node tx))

(defn submit-tx-await
  [node tx]
  (->> (submit-tx node tx)
       (xt/await-tx node)))

(defn ->txs
  [entities]
  (mapv (fn [node] [::xt/put node]) entities))

(defn ->entity
  [m identity-fn]
  (let [id (identity-fn m)]
    (assoc m :xt/id id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lifecycle

(defmethod ig/init-key ::xt-node [_ xtdb-opts]
  (log/info "Starting XT node")
  (xt/start-node xtdb-opts))

(defmethod ig/halt-key! ::xt-node [_ node]
  (.close node)
  (log/info "Closed XT node"))
