(ns quickrest.alpha.managers.specification.manager
  (:require [quickrest.alpha.utilities.schema :as us]
            [quickrest.alpha.engines.transformation.openapi-definitions :as eoas]
            [quickrest.alpha.resource-access.schema :as ras]
            [quickrest.alpha.resources.schema :as rsc]
            [quickrest.alpha.resources.system-specification :as rsy]
            [quickrest.alpha.resources.amos :as ram]
            [quickrest.alpha.resources.xtdb :as rxt]))

(defn transform
  [acquire-f transform-f input-schema target-schema]
  (let [spec (acquire-f)]
    (if-not (us/validate input-schema spec {})
      (merge (us/humanize input-schema spec {})
             {:workflow/failure :invalid-transformation-input})
      (let [transformed (transform-f spec)]
        (if-not (us/validate target-schema transformed {})
          (merge (us/humanize target-schema transformed {})
                 {:workflow/failure :invalid-transformation-output})
          transformed)))))

(def Source [:enum :source/file :source/url])

(defn transform-open-api-v2-spec-to-amos
  [xt-node source-name source-type spec-name]
  (let [raw-amos
        (transform
         (if (= source-type :source/file)
           #(rsy/open-api-from-file-path source-name)
           #(-> source-name
                rsy/open-api-from-url
                rsy/open-api-from-json-string))
         #(eoas/open-api-v2->amos (fn [_] (random-uuid)) spec-name %)
         eoas/OpenAPIV2Specification
         ram/AMOS)]
    (if (:workflow/failure raw-amos)
      (do
        ;;(println "WORKFLOW FAILURE")
        raw-amos)
      (let [amos-with-schema-nodes (rsc/merge-schema-nodes raw-amos)
            amos-entities          (->> amos-with-schema-nodes
                                        (ram/->XT-entities (fn [_] (random-uuid)))
                                        (rxt/->txs))
            amos-tx-result         (rxt/submit-tx-await xt-node amos-entities)
            schema-nodes-tx        (-> amos-with-schema-nodes
                                       (rsc/collect-schema-nodes)
                                       (rsc/schema-nodes->xt)
                                       (rsc/schema-nodes->tx))
            schema-nodes-tx-result (rxt/submit-tx-await xt-node schema-nodes-tx)]
        {:transformation/amos-id                (:amos/id raw-amos)
         :transformation/amos-tx-result         amos-tx-result
         :transformation/schema-modes-tx-result schema-nodes-tx-result
         :transformation/statistics             {:schema-nodes
                                                 (ras/count-nodes (rxt/db xt-node))}}))))


