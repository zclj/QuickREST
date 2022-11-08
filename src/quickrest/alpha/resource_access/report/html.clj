(ns quickrest.alpha.resource-access.report.html
  (:require [selmer.parser :as parser]
            [selmer.filters :as filters]
            [quickrest.alpha.resources.amos :as amos]
            [quickrest.alpha.repl :as repl]
            [quickrest.alpha.resources.exploration.result :as res]
            [quickrest.alpha.resources.xtdb :as rxt]
            [quickrest.alpha.resource-access.domain.rest :as rar]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PoC of HTML-reporting
;;
;; Produce a HTML-file report of operations with exploration result.
;; This implementation is a PoC and is not complete, it's an experiment.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(parser/set-resource-path!  (clojure.java.io/resource "templates"))

(defn render
  [template & [params]]
  (parser/render-file
   template
   (assoc params :page template)))

(defn report-page [params]
  (render "exploration.html" params))

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

(defn property->presentation-str
  [property]
  (cond
    (= (:property/key property) :property/state-mutation-property)
    "State mutation"
    :else
    "Example"))

(defn str-parameter
  [param]
  (str (:parameter/name param) " = " (first (vals (:parameter/value param)))))

(defn str-parameters
  [params]
  (clojure.string/join ", " (mapv str-parameter params)))

(defn operation->url
  [operation]
  (let [params  (:operation/parameters operation)
        parameter-map
        (or (apply merge (map :parameter/value (:operation/parameters operation)))
            {})
        request (rar/invoke-url (fn [r] r)
                                (merge operation
                                       {:http/scheme  :http
                                        :request/host "rest-go"
                                        :request/port 50100})
                                params
                                parameter-map)]
    (cond
      (= :get (:method request))
      (str "curl " (:url request))
      (= :post (:method request))
      (str "curl -X POST " (:url request))
      :else
      (str (:url request)))))

(defn property-description
  [property]
  (cond
    (= (:property/key property) :property/state-mutation-property)
    "The response of this operation is changed with the following example"
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
                    :else
                    {:bar
                     {:operation-sequence
                      (mapv pretty-operation (:exploration/example example))}}))})

(defn amos-data
  [db amos-id]
  (let [ops (amos/operations db amos-id [:operation/info
                                         :operation/parameters
                                         :http/method
                                         :http/url])
        examples
        (map #(select-keys
               %
               [:exploration/example :exploration/context :exploration/property])
             (res/results (repl/db) '[*]))
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
     (mapv (fn [o]
             {:name       (:info/name (:operation/info o))
              :method     (name (:http/method o))
              :url        (:http/url o)
              :parameters (mapv (fn [p]
                                  {:name   (:parameter/name p)
                                   :in     (name (:http/in p))
                                   :schema (:data/schema p)})
                                (:operation/parameters o))
              :examples   (mapv example->presentation (:operation/examples o))})
           ops-with-examples)}))

(comment
  (report-page {:foo "bar"})

  (def amos-id (:amos/id (ffirst (amos/amoses (repl/db) '[:amos/id]))))
  
  (amos-data
   (repl/db)
   (:amos/id (ffirst (amos/amoses (repl/db) '[:amos/id]))))

  (amos/operations (repl/db) amos-id '[*])
  
  (spit "report-out/report.html"
        (report-page (amos-data
                      (repl/db)
                      (:amos/id (ffirst (amos/amoses (repl/db) '[:amos/id]))))))

  ;; assoc operations with exploration results
  ;; find explorations for a operation
  (def examples (map #(select-keys % [:exploration/example :exploration/context]) (res/results (repl/db) '[*])))

  (mapv (fn [ex] (mapv :operation/info (:exploration/example ex))) examples)

  ;; This is the 'root' operation of the exploration
  (mapv (fn [ex] (:operation/info (:explored/operation (:exploration/context ex)))) examples)
  ;; for each operation in the spec, find any examples where it was the context
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
   (amos/operations (repl/db) amos-id '[*]))
  
  )
