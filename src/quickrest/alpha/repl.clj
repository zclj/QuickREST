(ns quickrest.alpha.repl
  (:require [malli.generator :as mg]
            [quickrest.alpha.main :as main]
            [xtdb.api :as xt]
            [quickrest.alpha.resources.xtdb :as rxt]
            [quickrest.alpha.managers.specification.manager :as sm]
            [quickrest.alpha.managers.objective.manager :as om]
            [quickrest.alpha.resources.exploration.result :as res]
            [quickrest.alpha.resource-access.report.pretty :as rp]
            [quickrest.alpha.resources.amos :as amos]))

(defn generate
  ([schema]
   (mg/generate schema nil))
  ([schema opts]
   (mg/generate schema opts)))

(defn system []
  main/system)

(defn config []
  (main/configuration))

(defn xt-node []
  (:quickrest.alpha.resources.xtdb/xt-node main/system))

(defn db []
  (xt/db (xt-node)))

(defn q
  [query & args]
  (apply rxt/q (db) query args))

(comment
  (q
   '{:find  [(pull e [*])]
     :where [[e :exploration/id]]})
  )
(defn put!
  [m]
  (rxt/submit-tx-await (xt-node) [[::xt/put m]]))

(def amos-id nil)

(defn transform-spec
  [file-name]
  (let [transform-result (sm/transform-open-api-v2-spec-to-amos
                          (xt-node) file-name :source/file "REPL spec")]
    (alter-var-root #'amos-id (constantly (:transformation/amos-id transform-result)))
    transform-result))

(defn transform-url-spec
  [url]
  (let [transform-result (sm/transform-open-api-v2-spec-to-amos
                          (xt-node) url :source/url "REPL spec")]
    (alter-var-root #'amos-id (constantly (:transformation/amos-id transform-result)))
    transform-result))

(def sut nil)

(defn set-sut
  [scheme port host]
  (let [sut-service {:http/scheme  scheme
                     :request/host host
                     :request/port port}]
    (alter-var-root #'sut (constantly sut-service))))

(def host "rest-go")

(comment
  (set-sut :http 8080 "rest-vm")
  )

(defn init-feature-service
  []
  (transform-spec "specifications/feature-service.json")
  (set-sut :http 50100 host))

(defn init-language-tool
  []
  (transform-spec "language-tool.json")
  (set-sut :http 8081 "rest-vm"))

(defn init-proxyprint-rest-go
  []
  (transform-spec "proxyprint.json")
  (set-sut :http 50105 host))

(defn init-ocvn-rest-go
  []
  (transform-spec "ocvn.json")
  (set-sut :http 50104 host))

(defn init-news-rest-go
  []
  (transform-spec "news.json")
  (set-sut :http 50103 host))

(defn init-scout-api-rest-go
  []
  (transform-spec "scout.json")
  (set-sut :http 50107 host))

(defn explore-properties
  [properties nrof-tests min-seq-size max-seq-size]
  (if (not (and amos-id sut))
    {:repl/error "amos id and sut must be set"}
    (om/explore-properties
     (xt-node)
     amos-id
     (merge sut {:nrof-tests nrof-tests :min-seq-size min-seq-size
                 :max-seq-size max-seq-size})
     properties)))

(comment
  (explore-properties [:response-equality] 10 0 0)
  (explore-properties [:state-mutation] 10 0 0)
  (explore-properties [:state-identity] 10 0 0)
  (explore-properties [:fuzz] 10 0 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Results

(defn results []
  (res/results (db) '[*]))

(defn result-ids []
  (res/results (db) '[:exploration/id]))

(defn pretty-report [{:keys [:exploration/id]}]
  (rp/report (db) id))

(defn pretty-reports []
  (mapv pretty-report (result-ids)))

(comment
  (defn prototype-report []
    (mapv
     #(quickrest.alpha.resource-access.report.next/report (db) (:exploration/id %))
     (result-ids)))
  )

(comment
  (amos/amoses (db) '[*])

  (amos/domain (db) amos-id)

  (amos/definitions (db) amos-id '[*])

  (amos/operations (db) amos-id '[*])

  (count (amos/operations (db) amos-id '[*]))
  )
