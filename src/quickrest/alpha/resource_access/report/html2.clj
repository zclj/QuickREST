(ns quickrest.alpha.resource-access.report.html2
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.tools.logging :as log]
            [quickrest.alpha.resource-access.domain.rest :as rar]
            [selmer.parser :as parser]
            [quickrest.alpha.resources.system-specification :as rsy]
            [quickrest.alpha.engines.transformation.openapi-definitions :as eoas]
            [quickrest.alpha.resource-access.domain.rest :as rst]
            [cheshire.core :as json]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PoC of HTML-reporting
;;
;; Produce a HTML-file report of operations with exploration result.
;; This implementation is a PoC and is not complete, it's an experiment.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML

(parser/set-resource-path!  (clojure.java.io/resource "templates"))

(defn render
  [template & [params]]
  (parser/render-file
   template
   (assoc params :page template)))

(defn report-page [params]
  (render "exploration.html" params))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data processing

(defn read-examples
  [filename]
  (try
    (with-open [r (io/reader filename)]
      (edn/read (java.io.PushbackReader. r)))
    (catch java.lang.IllegalArgumentException e
      (log/error (str "Could not open " filename " - " (.getMessage e)))
      (throw e))))

(def temp (atom []))

(defn operation->url
  [operation]
  (let [params      (:operation/parameters operation)
        parameter-map
        (or (apply merge (map
                          (fn [p]
                            {(:parameter/name p) (first (vals (:parameter/value p)))})
                          (:operation/parameters operation)))
            {})
        form-parameters
        (map (fn [p]
               {(:parameter/name p) (first (vals (:parameter/value p)))})
             (filter (fn [p] (= (:http/in p) :form-data))
                     (:operation/parameters operation)))
        raw-request (merge operation
                           {:http/scheme  :http
                            :request/host "server"
                            :request/port 3000})
        ;; _ (println (str "Parameter map : " parameter-map))
        ;; _ (println (str "Parameters :"))
        ;; _ (clojure.pprint/pprint params)
        ;; _ (println (str "Form params"))
        ;; _ (clojure.pprint/pprint form-parameters)
        ;; Use the invoke-url fn to resolve the parameters into an actual URL
        request     (-> (rar/make-http-request
                         raw-request
                         params
                         parameter-map)
                        (update :url
                                (fn [current-url]
                                  (-> current-url
                                      (rar/with-host raw-request)
                                      (rar/with-scheme raw-request)))))]
    (cond
      (= :get (:method request))
      (str "curl -X GET " (:url request))
      (= :post (:method request))
      (let [base (str "curl -X POST " (:url request))]
        (if (seq form-parameters)
          (str base " -H 'Content-Type: application/json' -d "
               (json/generate-string (apply merge form-parameters)))
          base))
      (= :put (:method request))
      (str "curl -X PUT " (:url request))
      (= :delete (:method request))
      (str "curl -X DELETE " (:url request))
      :else "Unsupported METHOD")))

(defn property->presentation-str
  [property]
  (cond
    (= (:property/key property) :property/state-mutation-property)
    "State mutation"
    (= (:property/key property) :property/state-identity-property)
    "State identity"
    (= (:property/key property) :property/response-equality-property)
    "Response equality"
    (= (:property/key property) :property/response-inequality-property)
    "Response inequality"
    :else
    "Unsupported"))

(defn property-description
  [property]
  (cond
    (= (:property/key property) :property/state-mutation-property)
    "The response of this operation is changed with the following example"
    (= (:property/key property) :property/response-equality-property)
    "Calling this operation multiple times produce the same response"
    (= (:property/key property) :property/response-inequality-property)
    "Calling this operation multiple times produce different responses"
    (= (:property/key property) :property/state-identity-property)
    "Calling this sequence bring the system back to the state of the first operation"
    :else ""))

(defn example->presentation
  [{:exploration/keys [property] :as example}]
  {:property    (property->presentation-str property)
   :description (property-description property)
   ;;:raw         example
   :example     (if (= (:operation/id (first (:exploration/example example)))
                       #uuid "deadbeef-dead-beef-dead-beef00000075")
                  {:no-example :found}
                  (mapv
                     (fn [ex]
                       (let [url (operation->url ex)]
                         {:url        url
                          :name       (:info/name (:operation/info ex))
                          :method     (clojure.string/upper-case (name (:http/method ex)))
                          :parameters (mapv (fn [p]
                                              {:name  (:parameter/name p)
                                               :in    (:http/in p)
                                               :value (:parameter/value p)})
                                            (:operation/parameters ex))}))
                     (:exploration/example example)))})

(defn report-property
  []
  (let [amos     (->> (rsy/open-api-from-file-path
                       "specifications/feature-service.json")
                      (eoas/open-api-v2->amos
                       (fn [_] (random-uuid)) "Feature Service"))
        ops      (:amos/operations amos)
        examples (concat (read-examples "raw-data/70/out/response-equality.edn")
                         (read-examples "raw-data/60/out/response-equality.edn")
                         (read-examples "raw-data/64/out/response-equality.edn")
                         (read-examples "raw-data/70/out/response-inequality.edn")
                         (read-examples "raw-data/60/out/response-inequality.edn")
                         (read-examples "raw-data/64/out/response-inequality.edn")
                         (read-examples "./out/response-equality.edn")
                         (read-examples "./out/state-mutation.edn")
                         (read-examples "raw-data/70/out/state-mutation.edn")
                         (read-examples "raw-data/60/out/state-mutation.edn")
                         (read-examples "raw-data/64/out/state-mutation.edn")
                         (read-examples "raw-data/70/out/state-identity.edn")
                         (read-examples "raw-data/60/out/state-identity.edn")
                         (read-examples "raw-data/64/out/state-identity.edn"))
        
        ops-with-examples
        (map
         (fn [o]
           (let [op-k (:info/key (:operation/info o))]
             (assoc
              o
              :operation/examples
              (filterv
               (fn [ex]
                 (= (:info/key (:operation/info (:explored/operation (:exploration/context ex))))
                    op-k))
               examples))))
         ops)]
    {:operations
     (sort-by
      :url
      (mapv (fn [o]
              {:name       (:info/name (:operation/info o))
               :method     (name (:http/method o))
               :url        (:http/url o)
               :parameters (set (mapv (fn [p]
                                        {:name (:parameter/name p)
                                         :in   (name (:http/in p))
                                         :schema
                                         (let [schema (:data/schema p)]
                                           (cond
                                             (and (= :map (first schema))
                                                  (= (count schema) 2))
                                             (name (last (last schema)))
                                             :else
                                             schema))})
                                      (:operation/parameters o)))
               :examples
               (sort-by
                :property
                (let [pexs (set (mapv example->presentation
                                      (:operation/examples o)))]
                  ;; Remove any empty examples where another instance has an example
                  (->> (group-by :property pexs)
                       (reduce-kv
                        (fn [acc prop exs]
                          ;; for each prop, 'no-example' only be included if size = 1
                          ;; NOTE - if there would be more 'no-example', they
                          ;; have already
                          ;; been removed by the 'set', hence size = 1 holds
                          (if (> (count exs) 1)
                            (concat
                             acc
                             (filterv
                              (fn [ex] (not= (:example ex) {:no-example :found})) exs))
                            (concat acc exs)))
                        [])
                       )
                  ))})
            ops-with-examples))}
    ))

(comment

  
  ;;;;
  ;; API
  (->> (report-property)
       (report-page)
       (spit "report-out/report2.html"))

  (def reported (report-property))

  (->> (group-by :property (:examples (nth (:operations reported) 1)))
       (reduce-kv
        (fn [acc prop exs]
          ;; for each prop, 'no-example' only be included if size = 1
          ;; NOTE - if there would be more 'no-example', they have already
          ;; been removed by the 'set', hence size = 1 holds
          (if (> (count exs) 1)
            (concat acc 
                    (filterv
                     (fn [ex] (not= (:example ex) {:no-example :found})) exs))
            (concat acc exs)))
        [])
       )
  ;;;;
  
  ;; base the reporting on the generated examples in the out folder

  (def raw-example (read-examples "raw-data/70/out/response-equality.edn"))

  (first (mapv keys raw-example))
  ;; => (:exploration/session :exploration/example :exploration/property :exploration/context :exploration/id :xt/id)

  ;; - We know the property from the file

  ;; - Process examples
  (def example (first raw-example))

  ;; @NOTE - :exploration/example is a vector of the operations in the example
  ;;  Different properties will need to 'root' there operations name in different ways
  ;;  ,i.e., which operation is the one 'owning' the example sequence
  (-> example :exploration/example first :operation/info :info/name)

  ;; Resolution of an operation and its paramters into an actual URL
  (-> example :exploration/example first operation->url)

  ;;;;
  ;; Operations
  (def feature-service-amos
    (->> (rsy/open-api-from-file-path "specifications/feature-service.json")
         (eoas/open-api-v2->amos (fn [_] (random-uuid)) "Feature Service")))

  (:amos/operations feature-service-amos)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Experiment with multi-merge

;; amos     (->> (rsy/open-api-from-file-path
;;                        "specifications/feature-service.json")
;;                       (eoas/open-api-v2->amos
;;                        (fn [_] (random-uuid)) "Feature Service"))
;;         ops      (:amos/operations amos)
;;         examples (into (read-examples "raw-data/70/out/response-equality.edn")
;;                        (read-examples "raw-data/70/out/state-mutation.edn"))

(defn amos-file-ops
  [path]
  (let [amos (->> (rsy/open-api-from-file-path path)
                  (eoas/open-api-v2->amos
                   (fn [_] (random-uuid)) "Feature Service"))
        ops  (:amos/operations amos)]
    ops))

(defn operations-with-examples
  [examples ops]
  (map
   (fn [o]
     (let [op-k (:info/key (:operation/info o))]
       (assoc
        o
        :operation/examples
        (filterv
         (fn [ex]
           (= (:info/key (:operation/info (:explored/operation (:exploration/context ex))))
              op-k))
         examples))))
   ops))

