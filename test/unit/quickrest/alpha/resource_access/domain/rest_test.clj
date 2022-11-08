(ns unit.quickrest.alpha.resource-access.domain.rest-test
  (:require [quickrest.alpha.resource-access.domain.rest :as sut]
            [clojure.test :refer [deftest testing is]]))

;; TODO : this obviously needs to improve, but do so when the REST resource is remade

(deftest param->http-param
  (is (= {:path     "path"
          :query    "query"
          :formData "formData"
          :body     "body"}
         (sut/param->http-param
          {"query"    "query"
           "path"     "path"
           "formData" "formData"
           "body"     "body"})))
  (is (= {}
         (sut/param->http-param {}))))

;;;;
;; Based on new AMOS format

(def example-operation
  {:operation/id   'amos-id
   :http/method    :post
   :http/url       "/products/{productName}/features/{featureName}"
   :operation/info {:info/name "addFeatureToProduct"
                    :info/key  :addFeatureToProduct}
   :operation/parameters
   [{:data/schema    [:map ["productName" :string]],
     :parameter/name "productName"
     :http/in        :path}
    {:data/schema    [:map ["featureName" :string]],
     :parameter/name "featureName"
     :http/in        :path}
    {:data/schema    [:map ["description" {:optional true} :string]],
     :parameter/name "description"
     :http/in        :form-data}
    {:data/schema    [:map ["page" {:optional true} :int]],
     :parameter/name "page"
     :http/in        :query}
    {:data/schema    [:map ["count" {:optional true} :int]],
     :parameter/name "count"
     :http/in        :query}]
   :operation/responses
   [{:data/schema [:map-of :int :any]}]})

(def example-parameter
  {})

(deftest make-http-request
  (is (= {:method      :post
          :url         "/products/a/features/b?page=1&count=2"
          :as          :json-string-keys
          :form-params {"description" "desc"}}
         (sut/make-http-request
          example-operation
          (:operation/parameters example-operation)
          {"productName" "a"
           "featureName" "b"
           "description" "desc"
           "page"        1
           "count"       2})))
  (testing "test for missing var in lookup table"
    (is (= {:method      :post
          :url         "/products/a/features/b?page=1&count=2"
          :as          :json-string-keys
          :form-params {}}
         (sut/make-http-request
          example-operation
          (:operation/parameters example-operation)
          {"productName" "a"
           "featureName" "b"
           "page"        1
           "count"       2}))))
  )

(def example-unresolved-request
  {:http/method          :get,
   :request/port         8080,
   :request/host         "hyper-mega",
   :operation/id         #uuid "f976c07b-65d5-4663-947c-3a7f1d6d25a6",
   :operation/info
   #:info{:name "getAllProducts", :key :getAllProducts},
   :operation/responses
   [{:http/status 200, :data/schema [:vector [:string]]}],
   :operation/parameters [],
   :http/scheme          :http,
   :http/url             "/products"})

(deftest should-invoke-http-function
  (let [received-request (atom nil)
        response         (sut/invoke-url
                          (fn [request]
                            (reset! received-request request)
                            {:status 200 :body ["Foo"]})
                          example-unresolved-request
                          []
                          {})]
    (testing "returns the response from invoke-fn"
      (is (= response {:status 200 :body ["Foo"]})))
    (testing "provides the invoke-fn with request"
      (is (= {:as :json-string-keys :method :get :url "http://hyper-mega:8080/products"}
             @received-request)))))

