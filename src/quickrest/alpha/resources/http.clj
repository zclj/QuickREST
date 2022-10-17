(ns quickrest.alpha.resources.http
  (:require [malli.util :as mu]))

(def HTTPScheme [:enum :http :https])

(def HTTPBasePath [:string])

(def HTTPContentType [:enum "application/json" "application/x-www-form-urlencoded"])

(def HTTPConsumes [:vector HTTPContentType])

(def HTTPStatus [:enum 200 201 202 204 404])

(def HTTPInStr [:enum "path" "formData" "query" "body"])

(def HTTPMethodStr [:enum "get" "delete" "put" "patch" "post"])

(def HTTPInKey [:enum :path :form-data :query :body])

(def HTTPMethodKey [:enum :get :delete :put :patch :post])

(def HTTPIn
  [:map
   [:http/in HTTPInKey]])

(def HTTPMethod
  [:map
   [:http/method HTTPMethodKey]])

(def HTTPURL
  [:map
   [:http/url [:string {:min 1}]]])

(def Request
  (mu/merge HTTPURL HTTPMethod))
