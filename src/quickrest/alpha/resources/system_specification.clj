(ns quickrest.alpha.resources.system-specification
  (:require [cheshire.core :as json]
            [clj-http.client :as http]))

(defn get-json-from-path
  [path]
  (json/parse-string (slurp path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open API
;;
;; The point here is to provide the raw OpenAPI document (json or yaml)

(defn open-api-from-file-path
  [path]
  (get-json-from-path path))

(defn open-api-from-json-string
  [json-str]
  (json/parse-string json-str))

(defn open-api-from-url
  ([url]
   (open-api-from-url
    url {:throw-exceptions false
         :insecure?        true}))
  ([url opts]
   (-> url (http/get opts) :body)))

