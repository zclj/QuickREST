(ns quickrest.alpha.resource-access.domain.rest
  (:require [clojure.string :as s]
            [clj-http.client :as http]
            [quickrest.alpha.resources.http :as rht]
            [quickrest.alpha.resources.amos :as amos]
            [clojure.tools.logging :as log]
            [quickrest.alpha.resources.amos :as ram]
            [malli.util :as mu]
            [malli.core :as m]))

(defn call-api
  [api-url {:keys [verb url body base-path protocol headers form-data] :as request}]
  (let [api-to-call (str protocol api-url base-path url)]
    (condp = (keyword verb)
      :get  (http/get
             api-to-call
             {:throw-exceptions false
              :headers          headers ;;{"Private-Token" token}
              :form-params      (or form-data {})
              :as               :json
              ;; TODO - pass this as an option
              :insecure?        true})
      :post (http/post
             api-to-call
             {:throw-exceptions false
              :headers          headers
              :form-params      form-data
              :content-type     :json
              :as               :json})
      :put  (http/put
             api-to-call
             {:throw-exceptions false
              :headers          headers
              :form-params      form-data
              :content-type     :json
              :as               :json})
      
      :delete (http/delete
               api-to-call
               {:throw-exceptions false
                :headers          headers
                :as               :json}))))

(defn build-path-vars
  [operation paths]
  (reduce-kv
   (fn [acc path-name path-value]
     (s/replace acc (str "{" path-name "}") (str path-value)))
   operation
   paths))

(defn build-request2
  [operation-id request-data]
  (let [operation-str (build-path-vars operation-id (:path request-data))
        query-str     (when (seq (:query request-data))
                    (clojure.string/join "&" (map #(clojure.string/join "=" %) (:query request-data))))]
    ;;(println (str "build-request with " request-data))
    (cond-> {:url operation-str
             :verb (:verb request-data)
             :base-path (:base-path request-data)}
      query-str                (update :url #(str % "?" query-str))
      (:formData request-data) (assoc :form-data (:formData request-data)))))

(defn param->http-param
  [param]
  (let [query     (get param "query")
        path      (get param "path")
        form-data (get param "formData")
        body      (get param "body")]
    (cond-> {}
      path      (merge {:path path})
      query     (merge {:query query})
      form-data (merge {:formData form-data})
      body      (merge {:body body}))))

(defn run-operation!
  ([amos token op-k params-m]
   (run-operation! amos token op-k params-m [:status :body :headers]))
  ([amos token op-k params-m select-ks]
   ;;(println (str "run-operation! " op-k " - " params-m))
   (let [op-name             (amos/find-in-spec amos op-k :operation/name)
         http-param          (param->http-param params-m)
         [verb path]         (clojure.string/split op-name #":")
         [path request-data] [path (merge {:verb (keyword verb)} http-param)]
         request             (merge (build-request2 path request-data)
                                    {:base-path "/api"
                                     :protocol  "http://" 
                                     :headers   {"Private-Token" token}})]
     (select-keys (call-api "hyper-mega" request) select-ks))))

(defn run-range-operation!
  [amos token op params]
  (let [range-spec     (amos/find-in-spec amos op :operation/range-spec)
        boundary-value (get-in (run-operation! amos token op params)
                               (:extraction range-spec))
        range-boundary (if boundary-value
                         (Integer/parseInt boundary-value)
                         1)]
      (reduce
       (fn [acc page]
         (conj acc (run-operation!
                    amos token
                    op
                    (assoc-in params (:path range-spec) page)
                    ;;[:status :body :headers]
                    [:status :body])))
       []
       (range 1 (+ range-boundary 1)))))

;; Refactor comments
;; - from the amos call we only seem to need the name of the operation
;; - however, the old amos had the PATH encoded in the name
;; - all HTTP conserns are now put in the HTTP layer
(defn run-operation-2!
  [amos token server headers base-path op-k params-m]
  (if (amos/find-in-spec amos op-k :operation/range-spec)
    (run-range-operation! amos token op-k params-m)
    (let [op-name             (amos/find-in-spec amos op-k :operation/name)
          http-param          (param->http-param params-m)
          [verb path]         (s/split op-name #":")
          [path request-data] [path (merge {:verb (keyword verb)} http-param)]
          request             (merge (build-request2 path request-data)
                                     {:base-path base-path
                                      :protocol  "http://" 
                                      :headers   headers})]
      (select-keys (call-api server request) [:status :body]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Execution

(def log-db (atom []))

(defn rest-dom
  [amos {:keys [token reset-fn] :as _context} operation param]
  ;;(println (str "Execute REST OP : " operation " - " param))
  (if operation
    (cond
      (= operation :reset) (do
                             ;;                       (println "RESET")
                             (reset! log-db [])
                             (when reset-fn
                               (reset-fn)))
      :else
      (let [api-token (or token "default-token")
            response  (run-operation-2! amos api-token operation param)]
        (swap! log-db conj [operation param response])
        response))
    :nop)
  )

(def RESTContext
  [:map
   [:token :string]
   [:reset-fn :any]
   [:server :string]
   [:headers :map]
   [:base-path :string]])

(defn rest-dom-v2
  {:malli/schema [:=> [:cat ram/AMOS RESTContext :keyword :map] :map]}
  [amos {:keys [token reset-fn server headers base-path] :as _context} operation param]
  (if operation
    (cond
      (= operation :reset) (do
                             ;;                       (println "RESET")
                             (reset! log-db [])
                             (when reset-fn
                               (reset-fn)))
      :else
      (let [api-token (or token "default-token")
            response  (run-operation-2! amos api-token server headers base-path operation param)]
        (swap! log-db conj [operation param response])
        response))
    :nop))

(defn execute-operation
  [{:keys [amos token reset-fn execution-delay] :as _ctx}
   {:keys [operation/key operation/parameter] :as meta-operation}]
  ;;(println (str "Operation => " meta-operation))
  ;;(println (str "Token => " token))
  ;;(println (str "reset-fn => " reset-fn))
  (print ".-")
  (assert parameter "No parameter generated")
  (if-not parameter
    {:no-op :nil-param}
    (let [{:keys [operation.key/value]} key
          parameter-value               (when parameter
                                          (:operation.parameter/value parameter))
          parameter-path
          (amos/find-in-spec amos value :operation/parameter-path)
          rest-value                    (cond
                                          (= parameter-path :query)
                                          {"query" parameter-value}
                                          (= parameter-path :path)
                                          {"path" parameter-value}
                                          (= parameter-path :formData)
                                          {"formData" parameter-value}
                                          :else parameter-value)]
      ;;(println (str "REST Value => " rest-value))
      (cond 
        (= value :reset)
        (do
          (println "\nRESET")
          (reset! log-db [])
          (when reset-fn
            (reset-fn)))
        :else (let [api-token (or token "default-token")
                    response  (run-operation-2! amos api-token value rest-value)]
                (swap! log-db conj [meta-operation response])
                (when (and (= (:status response) 202) execution-delay)
                  (do
                    (println "Async")
                    (Thread/sleep execution-delay)))
                response)))))


(defn make-execute-operation
  [server headers base-path]
  (fn
    [{:keys [amos token reset-fn execution-delay] :as _ctx}
     {:keys [operation/key operation/parameter] :as meta-operation}]
    ;;(println (str "Operation => " meta-operation))
    ;;(println (str "Token => " token))
    ;;(println (str "reset-fn => " reset-fn))
    (log/info "Execute Operation" meta-operation)
    (print ".-")
    (assert parameter "No parameter generated")
    (if-not parameter
      {:no-op :nil-param}
      (let [{:keys [operation.key/value]} key
            parameter-value               (when parameter
                                            (:operation.parameter/value parameter))
            parameter-path
            (amos/find-in-spec amos value :operation/parameter-path)
            rest-value                    (cond
                                            (= parameter-path :query)
                                            {"query" parameter-value}
                                            (= parameter-path :path)
                                            {"path" parameter-value}
                                            (= parameter-path :formData)
                                            {"formData" parameter-value}
                                            :else parameter-value)]
        ;;(println (str "REST Value => " rest-value))
        (cond 
          (= value :reset)
          (do
            (println "\nRESET")
            (reset! log-db [])
            (when reset-fn
              (reset-fn)))
          :else (let [api-token (or token "default-token")
                      response  (run-operation-2!
                                 amos api-token server headers base-path value rest-value)]
                  (swap! log-db conj [meta-operation response])
                  (when (and (= (:status response) 202) execution-delay)
                    (do
                      (println "Async")
                      (Thread/sleep execution-delay)))
                  response))))))

(defn report
  []
  @log-db)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adopt to new AMOS

;; (def RequestParts
;;   (mu/merge rht/HTTPURL rht/HTTPMethod rht/HTTPIn))

(def RequestParameters
  [:vector
   [:map
    [:http/in rht/HTTPInKey]
    [:parameter/name [:string {:min 1}]]]] )

(def Request
  [:map
   [:method rht/HTTPMethodKey]
   [:as [:enum :json :json-string-keys]]
   [:url [:string {:min 1}]]
   [:form-params {:optional true} :map]])

(defn resolve-url
  [unresolved-url params]
  (reduce-kv
   (fn [acc path-name path-value]
     (s/replace acc (str "{" path-name "}") (str path-value)))
   unresolved-url
   params))

(defn resolve-url-query-params
  [url params]
  (str url "?" (s/join "&" (map #(clojure.string/join "=" %) params))))

(comment
  (resolve-url-query-params "/foo" {"bar" "baz" "wingus" "dingus"})
  )

(defn make-http-request
  "Transforms an unresolved request to a resolved request. This means
  that the parameters are included in the the URL at thier position."
  {:malli/schema [:=> [:cat rht/Request RequestParameters :map] Request]}
  [{:http/keys [method url consumes base-path] :as _request}
   request-parameters parameters-lookup]
  (let [form-params  (filterv #(= (:http/in %) :form-data) request-parameters)
        path-params  (filterv #(= (:http/in %) :path) request-parameters)
        query-params (filterv #(= (:http/in %) :query) request-parameters)
        body-params  (filterv #(= (:http/in %) :body) request-parameters)
        to-map       (fn [params] (apply
                                   merge
                                   (map (fn [{:parameter/keys [name]}]
                                          (if-let [param-v (get parameters-lookup name)]
                                            {name param-v}
                                            {}))
                                        params)))]
    (cond-> {:method method
             :as     (if (seq consumes)
                       (keyword (name (keyword (first consumes))))
                       :json-string-keys)
             :url    (if (and base-path (not= "/" base-path))
                       (str base-path url)
                       url)}
      (seq path-params)  (assoc :url (resolve-url url (to-map path-params)))
      (seq query-params) (update :url resolve-url-query-params (to-map query-params))
      (seq body-params)  (assoc :body (to-map body-params))
      (seq form-params)
      (assoc :form-params (to-map form-params)))))

(def Response
  [:or
   [:map
    [:status rht/HTTPStatus]
    [:body :any]
    [:headers {:optional true} [:map-of :string :string]]]
   [:map
    [:execution/failure {:optional true} [:map]]]])

(def HTTPFn
  ;; TODO : define a response type
  [:=> [:cat Request] Response])

(def UnresolvedRequest
  (mu/merge
   rht/Request
   [:map
    [:request/host [:string {:min 1}]]
    [:http/scheme rht/HTTPScheme]
    [:http/base-path {:optional true} rht/HTTPBasePath]
    [:http/consumes {:optional true} rht/HTTPConsumes]
    [:request/port {:optional true} :int]
    [:request/base-path {:optional true} [:string {:min 1}]]
    [:http/headers {:optional true} [:map-of :string :string]]]))

(defn with-host
  [url {:request/keys [host port base-path]}]
  (cond->> url
    base-path (str base-path)
    port (str ":" port)
    host (str host)))

(comment
  (with-host
    "/foo" {:request/host "server" :request/port 8080 :request/base-path "/bar"})
  )

(defn with-scheme
  [url {:http/keys [scheme]}]
  (str (name scheme) "://" url))

(comment
  (->
   (with-host
     "/foo" {:request/host "server" :request/port 8080 :request/base-path "/bar"})
   (with-scheme
     {:http/scheme :http}))
  )

(defn validate-request
  [request request-parameters parameter-lookup]
  ;; all path params must be present in lookup and not empty
  ;;(log/debug (str "validate-request " request-parameters " : " parameter-lookup))
  (let [path-params (filterv #(= (:http/in %) :path) request-parameters)
        lookups     (map #(get parameter-lookup (:parameter/name %)) path-params)]
    (cond
      (not (every? identity lookups))     {:validation/message "Missing path parameters"
                                           :validation/reason
                                           {:parameter-lookup lookups
                                            :path-parameters  path-params}}
      (not (every? #(not= "" %) lookups)) {:validation/message "Empty path parameters"
                                           :validation/reason
                                           {:parameter-lookup lookups
                                            :path-parameters  path-params}}))
  )

;; TODO - add validation to make sure all the required path params are present
(defn invoke-url
  {:malli/schema [:=> [:cat HTTPFn UnresolvedRequest RequestParameters :map] Response]}
  [invoke-fn request request-parameters parameter-lookup]
  (if-let [validation-message
           (validate-request request request-parameters parameter-lookup)]
    {:execution/failure validation-message}
    (let [request-with-params (make-http-request
                               request request-parameters parameter-lookup)
          request-to-invoke   (update request-with-params :url
                                      (fn [current-url]
                                        (-> current-url
                                            (with-host request)
                                            (with-scheme request))))]
      (invoke-fn request-to-invoke))))
