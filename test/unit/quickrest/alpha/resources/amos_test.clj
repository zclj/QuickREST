(ns unit.quickrest.alpha.resources.amos-test
  (:require  [quickrest.alpha.resources.amos :as sut]
             [clojure.test :refer [deftest testing is]]))

(def example-amos
  #:amos{:domain
         #:domain{:meta-data
                  {"swagger"  "2.0",
                   "info"     {"version" "1.0"},
                   "host"     "localhost:8080",
                   "basePath" "/",
                   "schemes"  ["http"]}},
         :definitions
         #{#:data{:schema
                  [:map
                   ["id" [:int]]
                   ["name" [:string]]
                   ["features" [:set :definitions/Feature]]
                   ["constraints"
                    [:set :definitions/FeatureConstraint]]],
                  :name "Product",
                  :key  :definitions/Product}},
         :operations
         #{{:operation/id        #uuid "ea35a2aa-3c09-4ab5-b921-788dc2b844fc",
            :operation/info
            #:info{:name "deleteFeature", :key :deleteFeature},
            :operation/parameters
            [{:data/schema    [:map ["productName" :string]],
              :parameter/name "productName",
              :http/in        :path}
             {:data/schema    [:map ["configurationName" :string]],
              :parameter/name "configurationName",
              :http/in        :path}
             {:data/schema    [:map ["featureName" :string]],
              :parameter/name "featureName",
              :http/in        :path}
             {:data/schema    [:map ["productName" :string]],
              :parameter/name "productName",
              :http/in        :path}
             {:data/schema    [:map ["configurationName" :string]],
              :parameter/name "configurationName",
              :http/in        :path}],
            :operation/responses [#:data{:schema [:map-of :int :any]}],
            :http/method         :delete,
            :http/url
            "/products/{productName}/configurations/{configurationName}/features/{featureName}"}
           {:operation/id        #uuid "054cb443-841e-4566-89ac-8ddf2aae4378",
            :operation/info
            #:info{:name "addProduct", :key :addProduct},
            :operation/parameters
            [{:data/schema    [:map ["productName" :string]],
              :parameter/name "productName",
              :http/in        :path}],
            :operation/responses [#:data{:schema [:map-of :int :any]}],
            :http/method         :post,
            :http/url            "/products/{productName}"}},
         :name "Feature Spec",
         :id   #uuid "f30045ce-0cbc-4c58-b967-567d9bd74d4a"})

(deftest should-transform-to-XT
  (is (= [{:amos/id          #uuid "f30045ce-0cbc-4c58-b967-567d9bd74d4a"
           :xt/id            #uuid "f30045ce-0cbc-4c58-b967-567d9bd74d4a"
           :amos/name        "Feature Spec"
           :amos/operations  [#uuid "ea35a2aa-3c09-4ab5-b921-788dc2b844fc"
                              #uuid "054cb443-841e-4566-89ac-8ddf2aae4378"]
           :amos/definitions ["foo id"]
           :amos/domain      #:domain{:meta-data
                                      {"swagger"  "2.0",
                                       "info"     {"version" "1.0"},
                                       "host"     "localhost:8080",
                                       "basePath" "/",
                                       "schemes"  ["http"]}}}
          {:xt/id       "foo id"
           :data/schema [:map
                         ["id" [:int]]
                         ["name" [:string]]
                         ["features" [:set :definitions/Feature]]
                         ["constraints"
                          [:set :definitions/FeatureConstraint]]],
           :data/name   "Product",
           :data/key    :definitions/Product}
          {:xt/id               #uuid "ea35a2aa-3c09-4ab5-b921-788dc2b844fc",
           :operation/id        #uuid "ea35a2aa-3c09-4ab5-b921-788dc2b844fc",
           :operation/info
           #:info{:name "deleteFeature", :key :deleteFeature},
           :operation/parameters
           [{:data/schema    [:map ["productName" :string]],
             :parameter/name "productName",
             :http/in        :path}
            {:data/schema    [:map ["configurationName" :string]],
             :parameter/name "configurationName",
             :http/in        :path}
            {:data/schema    [:map ["featureName" :string]],
             :parameter/name "featureName",
             :http/in        :path}
            {:data/schema    [:map ["productName" :string]],
             :parameter/name "productName",
             :http/in        :path}
            {:data/schema    [:map ["configurationName" :string]],
             :parameter/name "configurationName",
             :http/in        :path}],
           :operation/responses [#:data{:schema [:map-of :int :any]}],
           :http/method         :delete,
           :http/url
           "/products/{productName}/configurations/{configurationName}/features/{featureName}"}
          {:xt/id               #uuid "054cb443-841e-4566-89ac-8ddf2aae4378"
           :operation/id        #uuid "054cb443-841e-4566-89ac-8ddf2aae4378",
           :operation/info
           #:info{:name "addProduct", :key :addProduct},
           :operation/parameters
           [{:data/schema    [:map ["productName" :string]],
             :parameter/name "productName",
             :http/in        :path}],
           :operation/responses [#:data{:schema [:map-of :int :any]}],
           :http/method         :post,
           :http/url            "/products/{productName}"}]
         (sut/->XT-entities (fn [_] "foo id") example-amos)))
  )
