(ns quickrest.alpha.resource-access.report.html2
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.tools.logging :as log]
            [quickrest.alpha.resource-access.domain.rest :as rar]
            [selmer.parser :as parser]
            [quickrest.alpha.resources.system-specification :as rsy]
            [quickrest.alpha.engines.transformation.openapi-definitions :as eoas]))

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

(defn operation->url
  [operation]
  (let [params  (:operation/parameters operation)
        parameter-map
        (or (apply merge (map :parameter/value (:operation/parameters operation)))
            {})
        ;; Use the invoke-url fn to resolve the parameters into an actual URL
        request (rar/invoke-url (fn [r]
                                  ;; Just echo the request
                                  (println "REQUEST")
                                  (clojure.pprint/pprint r)
                                  r)
                                (merge operation
                                       {:http/scheme  :http
                                        :request/host "server"
                                        :request/port 3000})
                                params
                                parameter-map)]
    (cond
      (= :get (:method request))
      (str "curl " (:url request))
      (= :post (:method request))
      (str "curl -X POST " (:url request))
      (= :put (:method request))
      (str "curl -X PUT " (:url request))
      (= :delete (:method request))
      (str "curl -X DELETE " (:url request))
      :else
      (do
        (println "Unsupported")
        (clojure.pprint/pprint request)
        (clojure.pprint/pprint operation)
        (str "Unsupported : " (:method request))))))

(defn property->presentation-str
  [property]
  (cond
    (= (:property/key property) :property/state-mutation-property)
    "State mutation"
    (= (:property/key property) :property/response-equality-property)
    "Response equality"
    :else
    "Unsupported"))

(defn property-description
  [property]
  (cond
    (= (:property/key property) :property/state-mutation-property)
    "The response of this operation is changed with the following example"
    (= (:property/key property) :property/response-equality-property)
    "Calling this operation multiple times produce the same response"
    :else ""))

(defn example->presentation
  [{:exploration/keys [property] :as example}]
  {:property    (property->presentation-str property)
   :description (property-description property)
   :raw         example
   :example     (if (= (:operation/id (first (:exploration/example example)))
                       #uuid "deadbeef-dead-beef-dead-beef00000075")
                  {:no-example :found}
                  (cond
                    (= (:property/key property) :property/state-mutation-property)
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
                     (:exploration/example example))
                    (= (:property/key property) :property/response-equality-property)
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
                     (:exploration/example example))
                    :else 
                    (println "Unsupported property")))})

(defn report-property
  []
  (let [amos     (->> (rsy/open-api-from-file-path
                       "specifications/feature-service.json")
                      (eoas/open-api-v2->amos
                       (fn [_] (random-uuid)) "Feature Service"))
        ops      (:amos/operations amos)
        examples (into (read-examples "raw-data/70/out/response-equality.edn")
                       (read-examples "raw-data/70/out/state-mutation.edn"))
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
    ;;{:examples (mapv example->presentation examples)}
    {:operations
     (mapv (fn [o]
             {:name       (:info/name (:operation/info o))
              :method     (name (:http/method o))
              :url        (:http/url o)
              :parameters (mapv (fn [p]
                                  {:name   (:parameter/name p)
                                   :in     (name (:http/in p))
                                   :schema
                                   (let [schema (:data/schema p)]
                                     (cond
                                       (and (= :map (first schema))
                                            (= (count schema) 2))
                                       (name (last (last schema)))
                                       :else
                                       schema))})
                                (:operation/parameters o))
              :examples   (mapv example->presentation (:operation/examples o))})
           ops-with-examples)}
    ))

(comment

  
  ;;;;
  ;; API
  (->> (report-property)
       (report-page)
       (spit "report-out/report2.html"))
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


