(ns unit.quickrest.alpha.engines.transformation.openapi-definitions-test
  (:require [clojure.test :refer [deftest testing is]]
            [quickrest.alpha.engines.transformation.openapi-definitions :as sut]
            [quickrest.alpha.resources.system-specification :as sys]
            [malli.dev :as dev]
            [malli.dev.pretty :as pretty]
            [quickrest.alpha.repl :as repl]
            [malli.core :as m]
            [malli.error :as me]
            [quickrest.alpha.resources.amos :as amos]
            [malli.generator :as mg]))

(dev/start! {:report (pretty/reporter)})

(deftest should-transform-OpenAPI-V2-meta-data-to-AMOS
  (testing "Transform the meta-data, such as 'info', 'host', 'basePath', etc into an AMOS 'storage'"
    (is (= {:amos/domain {:domain/meta-data
                          {"swagger"  "2.0"
                           "info"     {"version" "1.0"}
                           "host"     "localhost:8080"
                           "basePath" "/"
                           "schemes"  ["http"]}}}
           (sut/open-api-v2-meta-data->amos
            {"swagger"  "2.0"
             "info"     {"version" "1.0"}
             "host"     "localhost:8080"
             "basePath" "/"
             "schemes"  ["http"]})))))

(deftest open-api-parameter-type->AMOS-type
  (testing "transform 'string' type"
    (is (= :string (sut/open-api-parameter-type->AMOS-type "string"))))
  (testing "transform 'integer' type"
    (is (= :int (sut/open-api-parameter-type->AMOS-type "integer"))))
  (testing "transform 'number' type"
    (is (= :double (sut/open-api-parameter-type->AMOS-type "number"))))
  (testing "transform 'boolean' type"
    (is (= :boolean (sut/open-api-parameter-type->AMOS-type "boolean"))))
  ;; TODO - move this to some negative testing ns
  ;; (testing "transform unknown type yields any"
  ;;   (is (= :any (sut/open-api-parameter-type->AMOS-type "foo"))))
  )

(deftest should-transform-OpenAPI-V2-parameter-to-AMOS-operations
  (testing "path parameter"
    (is (= {:data/schema    [:map
                             ["productName" :string]]
            :parameter/name "productName"
            :http/in        :path}
           (sut/open-api-v2-parameter->amos
            {"name"     "productName",
             "in"       "path",
             "required" true,
             "type"     "string"}))))
  (testing "not required are reflected in the schema"
    (is (= {:data/schema    [:map
                             ["productName" {:optional true} :string]]
            :parameter/name "productName"
            :http/in        :path}
           (sut/open-api-v2-parameter->amos
            {"name"     "productName",
             "in"       "path",
             "required" false,
             "type"     "string"}))))
  (testing "formData parameter"
    (is (= {:data/schema    [:map
                             ["description" {:optional true} :string]]
            :parameter/name "description"
            :http/in        :form-data}
           (sut/open-api-v2-parameter->amos
            {"name"     "description",
             "in"       "formData",
             "required" false,
             "type"     "string"}))))
  (testing "query parameter"
    (is (= {:data/schema    [:map ["statistics" {:optional true} :boolean]]
            :parameter/name "statistics"
            :http/in        :query}
           (sut/open-api-v2-parameter->amos
            {"description" "Include project statistics"
             "in"          "query"
             "name"        "statistics"
             "required"    false
             "type"        "boolean"}))))
  (testing "query parameter"
    (is (= {:data/schema    [:map ["file" :any]]
            :parameter/name "file"
            :http/in        :form-data}
           (sut/open-api-v2-parameter->amos
            {"description" "The file to be uploaded"
             "in"          "formData"
             "name"        "file"
             "required"    true
             "type"        "file"}))))
  (testing "Out of spec items"
    (is (= {:data/schema    [:map ["actions" :any]]
            :parameter/name "actions"
            :http/in        :form-data}
           (sut/open-api-v2-parameter->amos
            {"description" "Actions to perform in commit"
             "in"          "formData"
             "items"       {"x-type" "object"}
             "name"        "actions"
             "required"    true
             "type"        "array"})))))

(deftest should-transform-OpenAPI-V2-path-to-AMOS-operations
  (testing "Transform parameters to AMOS format"
    (is (= [{:data/schema    [:map ["productName" :string]],
             :parameter/name "productName",
             :http/in        :path}
            {:data/schema    [:map ["featureName" :string]],
             :parameter/name "featureName",
             :http/in        :path}
            {:data/schema    [:map ["description" {:optional true} :string]],
             :parameter/name "description",
             :http/in        :form-data}]
           (sut/open-api-v2-parameters->amos
            {"parameters"
             [{"name"     "productName",
               "in"       "path",
               "required" true,
               "type"     "string"}
              {"name"     "featureName",
               "in"       "path",
               "required" true,
               "type"     "string"}
              {"name"     "description",
               "in"       "formData",
               "required" false,
               "type"     "string"}]})))))

(deftest transform-OpenAPI-V2-response-to-AMOS-schema
  (testing "default response"
    (is (= {:data/schema [:map-of :int :any]}
           (sut/open-api-v2-response->amos
            {"default" {"description" "successful operation"}}))))
  (testing "200"
    (is (= {:data/schema [:ref :definitions/Feature]
            :http/status 200}
           (sut/open-api-v2-response->amos
            {"200"
             {"description" "successful operation",
              "schema"      {"$ref" "#/definitions/Feature"},
              "headers"     {}}}))))
  (testing "array items response"
    (is (= {:data/schema [:vector [:string]]
            :http/status 200}
           (sut/open-api-v2-response->amos
            {"200"
             {"description" "successful operation",
              "schema"      {"type" "array", "items" {"type" "string"}},
              "headers"     {}}}))))
  (testing "status code is parsed"
    (is (= {:data/schema [:vector [:string]]
            :http/status 404}
           (sut/open-api-v2-response->amos
            {"404"
             {"description" "successful operation",
              "schema"      {"type" "array", "items" {"type" "string"}},
              "headers"     {}}}))))
  (testing "response might not have a schema"
    (is (= {:http/status 200
            :data/schema :any}
           (sut/open-api-v2-response->amos
            {"200"
             {"description" "Set emails-on-push service for project"}})))))

(deftest transform-responses
  (testing "transform map of responses"
    (is (= [{:data/schema [:map-of :int :any]}]
           (sut/open-api-v2-responses->amos
            {"responses" {"default" {"description" "successful operation"}}})))))

(deftest transform-name
  (testing "transform operationId to name and key"
    (is (= {:info/name     "addFeatureToProduct"
            :info/key      :addFeatureToProduct
            :info/produces "application/json"}
           (sut/open-api-v2-operation-id->amos
            "localhost"
            "get"
            {"operationId" "addFeatureToProduct"
             "produces"    "application/json"}))))
  ;; TODO - this test is 'out of spec', is that relevant?
  ;; (testing "no operationId yields nil"
  ;;   (is (nil?
  ;;        (sut/open-api-v2-operation-id->amos
  ;;         {"foo" "bar"}))))
  )

(deftest transform-path-entry
  (let [id (random-uuid)]
    (is (= {:operation/id   id 
            :operation/info {:info/name     "addFeatureToProduct"
                             :info/key      :addFeatureToProduct
                             :info/produces ["application/json"]}
            :operation/parameters
            [{:data/schema    [:map ["productName" :string]],
              :parameter/name "productName"
              :http/in        :path}
             {:data/schema    [:map ["featureName" :string]],
              :parameter/name "featureName"
              :http/in        :path}
             {:data/schema    [:map ["description" {:optional true} :string]],
              :parameter/name "description"
              :http/in        :form-data}]
            :operation/responses
            [{:data/schema [:map-of :int :any]}]}
           (sut/open-api-v2-path-entry->amos
            "localhost"
            id
            "get"
            {"operationId" "addFeatureToProduct",
             "produces"    ["application/json"],
             "parameters"
             [{"name"     "productName",
               "in"       "path",
               "required" true,
               "type"     "string"}
              {"name"     "featureName",
               "in"       "path",
               "required" true,
               "type"     "string"}
              {"name"     "description",
               "in"       "formData",
               "required" false,
               "type"     "string"}],
             "responses"   {"default" {"description" "successful operation"}}})))))

(deftest transform-path
  (let [id (random-uuid)]
    (is (= {:operation/id   id
            :http/method    :post
            :operation/info {:info/name     "addFeatureToProduct"
                             :info/key      :addFeatureToProduct
                             :info/produces ["application/json"]}
            :operation/parameters
            [{:data/schema    [:map ["productName" :string]],
              :parameter/name "productName",
              :http/in        :path}
             {:data/schema    [:map ["featureName" :string]],
              :parameter/name "featureName",
              :http/in        :path}
             {:data/schema    [:map ["description" {:optional true} :string]],
              :parameter/name "description",
              :http/in        :form-data}]
            :operation/responses
            [{:data/schema [:map-of :int :any]}]}
           (sut/open-api-v2-path->amos
            "localhost"
            id
            "post"
            {"operationId" "addFeatureToProduct",
             "produces"    ["application/json"],
             "parameters"
             [{"name"     "productName",
               "in"       "path",
               "required" true,
               "type"     "string"}
              {"name"     "featureName",
               "in"       "path",
               "required" true,
               "type"     "string"}
              {"name"     "description",
               "in"       "formData",
               "required" false,
               "type"     "string"}],
             "responses"   {"default" {"description" "successful operation"}}})))))

(deftest transform-methods
  (let [id (random-uuid)]
    (is (= #{{:operation/id        id
              :operation/info
              #:info{:name     "deleteFeatureOfProduct",
                     :key      :deleteFeatureOfProduct
                     :produces ["application/json"]},
              :operation/parameters
              [{:data/schema    [:map ["productName" :string]],
                :parameter/name "productName",
                :http/in        :path}
               {:data/schema    [:map ["featureName" :string]],
                :parameter/name "featureName",
                :http/in        :path}],
              :operation/responses [#:data{:schema [:map-of :int :any]}],
              :http/method         :delete,
              :http/url            "the/url"}
             {:operation/id        id
              :operation/info
              #:info{:name     "addFeatureToProduct"
                     :key      :addFeatureToProduct
                     :produces ["application/json"]},
              :operation/parameters
              [{:data/schema    [:map ["productName" :string]],
                :parameter/name "productName",
                :http/in        :path}
               {:data/schema    [:map ["featureName" :string]],
                :parameter/name "featureName",
                :http/in        :path}
               {:data/schema    [:map ["description" {:optional true} :string]],
                :parameter/name "description",
                :http/in        :form-data}],
              :operation/responses [#:data{:schema [:map-of :int :any]}],
              :http/method         :post,
              :http/url            "the/url"}
             {:operation/id id
              :operation/info
              #:info{:name     "updateFeatureOfProduct",
                     :key      :updateFeatureOfProduct
                     :produces ["application/json"]},
              :operation/parameters
              [{:data/schema    [:map ["productName" :string]],
                :parameter/name "productName",
                :http/in        :path}
               {:data/schema    [:map ["featureName" :string]],
                :parameter/name "featureName",
                :http/in        :path}
               {:data/schema    [:map ["description" {:optional true} :string]],
                :parameter/name "description",
                :http/in        :form-data}],
              :operation/responses
              [{:http/status 200, :data/schema [:ref :definitions/Feature]}],
              :http/method  :put,
              :http/url     "the/url"}}
           (sut/open-api-v2-methods->amos
            (fn [_] id)
            "the/url"
            {"post"
             {"operationId" "addFeatureToProduct",
              "produces"    ["application/json"],
              "parameters"
              [{"name"     "productName",
                "in"       "path",
                "required" true,
                "type"     "string"}
               {"name"     "featureName",
                "in"       "path",
                "required" true,
                "type"     "string"}
               {"name"     "description",
                "in"       "formData",
                "required" false,
                "type"     "string"}],
              "responses"   {"default" {"description" "successful operation"}}},
             "put"
             {"operationId" "updateFeatureOfProduct",
              "produces"    ["application/json"],
              "parameters"
              [{"name"     "productName",
                "in"       "path",
                "required" true,
                "type"     "string"}
               {"name"     "featureName",
                "in"       "path",
                "required" true,
                "type"     "string"}
               {"name"     "description",
                "in"       "formData",
                "required" false,
                "type"     "string"}],
              "responses"
              {"200"
               {"description" "successful operation",
                "schema"      {"$ref" "#/definitions/Feature"},
                "headers"     {}}}},
             "delete"
             {"operationId" "deleteFeatureOfProduct",
              "produces"    ["application/json"],
              "parameters"
              [{"name"     "productName",
                "in"       "path",
                "required" true,
                "type"     "string"}
               {"name"     "featureName",
                "in"       "path",
                "required" true,
                "type"     "string"}],
              "responses"   {"default" {"description" "successful operation"}}}})))))

(deftest transform-paths
  (let [id (random-uuid)]
    (is (= #{{:operation/id        id
              :operation/info
              #:info{:name     "addFeatureToProduct"
                     :key      :addFeatureToProduct
                     :produces ["application/json"]},
              :operation/parameters
              [{:data/schema    [:map ["productName" :string]],
                :parameter/name "productName",
                :http/in        :path}
               {:data/schema    [:map ["featureName" :string]],
                :parameter/name "featureName",
                :http/in        :path}
               {:data/schema    [:map ["description" {:optional true} :string]],
                :parameter/name "description",
                :http/in        :form-data}],
              :operation/responses [#:data{:schema [:map-of :int :any]}],
              :http/method         :post,
              :http/url            "/products/{productName}/features/{featureName}"}
             {:operation/id        id
              :operation/info
              #:info{:name     "deleteFeatureOfProduct",
                     :key      :deleteFeatureOfProduct
                     :produces ["application/json"]},
              :operation/parameters
              [{:data/schema    [:map ["productName" :string]],
                :parameter/name "productName",
                :http/in        :path}
               {:data/schema    [:map ["featureName" :string]],
                :parameter/name "featureName",
                :http/in        :path}],
              :operation/responses [#:data{:schema [:map-of :int :any]}],
              :http/method         :delete,
              :http/url            "/products/{productName}/features/{featureName}"}
             {:operation/id id
              :operation/info
              #:info{:name     "updateFeatureOfProduct",
                     :key      :updateFeatureOfProduct
                     :produces ["application/json"]},
              :operation/parameters
              [{:data/schema    [:map ["productName" :string]],
                :parameter/name "productName",
                :http/in        :path}
               {:data/schema    [:map ["featureName" :string]],
                :parameter/name "featureName",
                :http/in        :path}
               {:data/schema    [:map ["description" {:optional true} :string]],
                :parameter/name "description",
                :http/in        :form-data}],
              :operation/responses
              [{:http/status 200, :data/schema [:ref :definitions/Feature]}],
              :http/method  :put,
              :http/url     "/products/{productName}/features/{featureName}"}}
           (sut/open-api-v2-paths->amos
            (fn [_] id)
            {"/products/{productName}/features/{featureName}"
             {"post"
              {"operationId" "addFeatureToProduct",
               "produces"    ["application/json"],
               "parameters"
               [{"name"     "productName",
                 "in"       "path",
                 "required" true,
                 "type"     "string"}
                {"name"     "featureName",
                 "in"       "path",
                 "required" true,
                 "type"     "string"}
                {"name"     "description",
                 "in"       "formData",
                 "required" false,
                 "type"     "string"}],
               "responses"   {"default" {"description" "successful operation"}}},
              "put"
              {"operationId" "updateFeatureOfProduct",
               "produces"    ["application/json"],
               "parameters"
               [{"name"     "productName",
                 "in"       "path",
                 "required" true,
                 "type"     "string"}
                {"name"     "featureName",
                 "in"       "path",
                 "required" true,
                 "type"     "string"}
                {"name"     "description",
                 "in"       "formData",
                 "required" false,
                 "type"     "string"}],
               "responses"
               {"200"
                {"description" "successful operation",
                 "schema"      {"$ref" "#/definitions/Feature"},
                 "headers"     {}}}},
              "delete"
              {"operationId" "deleteFeatureOfProduct",
               "produces"    ["application/json"],
               "parameters"
               [{"name"     "productName",
                 "in"       "path",
                 "required" true,
                 "type"     "string"}
                {"name"     "featureName",
                 "in"       "path",
                 "required" true,
                 "type"     "string"}],
               "responses"   {"default" {"description" "successful operation"}}}}})))))

(deftest transform-definition
  (testing "Basic object definition"
    (is (= {:data/name "Foo"
            :data/key  :definitions/Foo
            :data/schema
            [:map
             ["id" [:int]]
             ["on" [:boolean]]
             ["name" [:string]]
             ["description" [:string]]]}
           (sut/open-api-v2-definition->amos
            "Foo"
            {"type" "object",
             "properties"
             {"id"          {"type" "integer", "format" "int64"},
              "on"          {"type" "boolean"}
              "name"        {"type" "string"},
              "description" {"type" "string"}}}))))
  (testing "Object with no properties (yes, it happens in the wild)"
    (is (= {:data/name   "Foo"
            :data/key    :definitions/Foo
            :data/schema [:map]}
           (sut/open-api-v2-definition->amos
            "Foo"
            {"type" "object"}))))
  (testing "Definition with references"
    (is (= {:data/name "Foo"
            :data/key  :definitions/Foo
            :data/schema
            [:map
             ["id" [:int]]
             ["name" [:string]]
             ["features"
              [:vector {:json-schema/unique-items true} :definitions/Feature]]
             ["constraints"
              [:vector
               {:json-schema/unique-items true}
               :definitions/FeatureConstraint]]]}
           (sut/open-api-v2-definition->amos
            "Foo"
            {"type" "object",
             "properties"
             {"id"   {"type" "integer", "format" "int64"},
              "name" {"type" "string"},
              "features"
              {"type"        "array",
               "readOnly"    true,
               "uniqueItems" true,
               "items"       {"$ref" "#/definitions/Feature"}},
              "constraints"
              {"type"        "array",
               "readOnly"    true,
               "uniqueItems" true,
               "items"       {"$ref" "#/definitions/FeatureConstraint"}}}})))))

(deftest transform-definitions
  (is (= #{{:data/name "Feature"
            :data/key  :definitions/Feature
            :data/schema
            [:map
             ["id" [:int]]
             ["name" [:string]]
             ["description" [:string]]]}
           {:data/name "FeatureConstraint"
            :data/key  :definitions/FeatureConstraint
            :data/schema
            [:map
             ["id" [:int]]
             ["type" [:string]]]}
           {:data/name "Product"
            :data/key  :definitions/Product
            :data/schema
            [:map
             ["id" [:int]]
             ["name" [:string]]
             ["features" [:vector {:json-schema/unique-items true} :definitions/Feature]]
             ["constraints"
              [:vector
               {:json-schema/unique-items true}
               :definitions/FeatureConstraint]]]}
           {:data/name   "ProductsConfigurationResource"
            :data/key    :definitions/ProductsConfigurationResource
            :data/schema [:map]}
           {:data/name   "ProductsConfigurationFeaturesResource"
            :data/key    :definitions/ProductsConfigurationFeaturesResource
            :data/schema [:map]}
           {:data/name "ProductConfiguration"
            :data/key  :definitions/ProductConfiguration
            :data/schema
            [:map
             ["name" [:string]]
             ["valid" [:boolean]]
             ["activedFeatures"
              [:vector {:json-schema/unique-items true} :definitions/Feature]]]}
           {:data/name   "ProductsFeaturesResource"
            :data/key    :definitions/ProductsFeaturesResource
            :data/schema [:map]}
           {:data/name   "ProductsConstraintsResource"
            :data/key    :definitions/ProductsConstraintsResource
            :data/schema [:map]}}
         (sut/open-api-v2-definitions->amos
          {"Feature"
           {"type" "object",
            "properties"
            {"id"          {"type" "integer", "format" "int64"},
             "name"        {"type" "string"},
             "description" {"type" "string"}}},
           "FeatureConstraint"
           {"type" "object",
            "properties"
            {"id"   {"type" "integer", "format" "int64"},
             "type" {"type" "string"}}},
           "Product"
           {"type" "object",
            "properties"
            {"id"   {"type" "integer", "format" "int64"},
             "name" {"type" "string"},
             "features"
             {"type"        "array",
              "readOnly"    true,
              "uniqueItems" true,
              "items"       {"$ref" "#/definitions/Feature"}},
             "constraints"
             {"type"        "array",
              "readOnly"    true,
              "uniqueItems" true,
              "items"       {"$ref" "#/definitions/FeatureConstraint"}}}},
           "ProductsConfigurationResource"         {"type" "object"},
           "ProductsConfigurationFeaturesResource" {"type" "object"},
           "ProductConfiguration"
           {"type" "object",
            "properties"
            {"name"  {"type" "string"},
             "valid" {"type" "boolean"},
             "activedFeatures"
             {"type"        "array",
              "uniqueItems" true,
              "items"       {"$ref" "#/definitions/Feature"}}}},
           "ProductsFeaturesResource"              {"type" "object"},
           "ProductsConstraintsResource"           {"type" "object"}}))))

(deftest transform-open-api-v2->amos
  (let [amos-id (random-uuid)]
    (is (= {:amos/id     amos-id
            :amos/name   "Foo"
            :amos/domain {:domain/meta-data {"swagger"  "2.0"
                                             "info"     {"version" "1.0"}
                                             "host"     "localhost:8080"
                                             "basePath" "/"
                                             "schemes"  ["http"]}}
            :amos/operations
            #{{:operation/id   amos-id
               :http/method    :post
               :http/url       "/products/{productName}/features/{featureName}"
               :operation/info {:info/name     "addFeatureToProduct"
                                :info/key      :addFeatureToProduct
                                :info/produces ["application/json"]}
               :operation/parameters
               [{:data/schema    [:map ["productName" :string]],
                 :parameter/name "productName"
                 :http/in        :path}
                {:data/schema    [:map ["featureName" :string]],
                 :parameter/name "featureName",
                 :http/in        :path}
                {:data/schema    [:map ["description" {:optional true} :string]],
                 :parameter/name "description",
                 :http/in        :form-data}]
               :operation/responses
               [{:data/schema [:map-of :int :any]}]}}
            :amos/definitions
            #{{:data/name "Feature"
               :data/key  :definitions/Feature
               :data/schema
               [:map
                ["id" [:int]]
                ["name" [:string]]
                ["description" [:string]]]}}}
           (sut/open-api-v2->amos
            (fn [_] amos-id)
            "Foo"
            {"swagger"  "2.0"
             "info"     {"version" "1.0"}
             "host"     "localhost:8080"
             "basePath" "/"
             "schemes"  ["http"]
             "paths"
             {"/products/{productName}/features/{featureName}"
              {"post"
               {"operationId" "addFeatureToProduct",
                "produces"    ["application/json"],
                "parameters"
                [{"name"     "productName",
                  "in"       "path",
                  "required" true,
                  "type"     "string"}
                 {"name"     "featureName",
                  "in"       "path",
                  "required" true,
                  "type"     "string"}
                 {"name"     "description",
                  "in"       "formData",
                  "required" false,
                  "type"     "string"}],
                "responses"   {"default" {"description" "successful operation"}}}}}
             "definitions"
             {"Feature"
              {"type" "object",
               "properties"
               {"id"          {"type" "integer", "format" "int64"},
                "name"        {"type" "string"},
                "description" {"type" "string"}}}}}))))
  )

;; TODO - move to integration tests
(defn try-gen
  [definitions schema-source-k schema-k operation]
  (let [schemas (map schema-k (get operation schema-source-k))]
    (mapv
     #(mg/generate % {:registry (merge (m/default-schemas) definitions)
                      :size 2})
     schemas)))

(defn try-gen-operations
  [definitions operations]
  (mapv
   (fn [op]
     [(try-gen definitions :operation/responses :data/schema op)
      (try-gen definitions :operation/parameters :data/schema op)])
   operations))

(deftest feature-spec
  (let [feature-spec (-> "specifications/feature-service.json"
                         (sys/open-api-from-file-path))
        amos         (sut/open-api-v2->amos
                      (fn [_] (random-uuid)) "Feature Spec"  feature-spec)]
    (testing "transformed spec should comply to AMOS format"
      (is (= true (m/validate amos/AMOS amos))))
    (testing "defintions are valid schemas"
      (is (= #:definitions{:Feature
                           [:map
                            ["id" [:int]]
                            ["name" [:string]]
                            ["description" [:string]]],
                           :ProductsConstraintsResource           [:map],
                           :ProductsConfigurationFeaturesResource [:map],
                           :FeatureConstraint
                           [:map ["id" [:int]] ["type" [:string]]],
                           :ProductsFeaturesResource              [:map],
                           :Product
                           [:map
                            ["id" [:int]]
                            ["name" [:string]]
                            ["features"
                             [:vector
                              {:json-schema/unique-items true}
                              :definitions/Feature]]
                            ["constraints"
                             [:vector
                              {:json-schema/unique-items true}
                              :definitions/FeatureConstraint]]],
                           :ProductConfiguration
                           [:map
                            ["name" [:string]]
                            ["valid" [:boolean]]
                            ["activedFeatures"
                             [:vector
                              {:json-schema/unique-items true}
                              :definitions/Feature]]],
                           :ProductsConfigurationResource         [:map]}
             (amos/make-definitions-index amos))))
    (testing "schemas can generate with defintions"
      (is (= true
             (vector?
              (try-gen-operations (amos/make-definitions-index amos)
                                  (:amos/operations amos))))))))

(deftest gitlab-spec
  (let [gitlab-spec (-> "specifications/gitlab-v3.json"
                        (sys/open-api-from-file-path))
        amos        (sut/open-api-v2->amos
                     (fn [_] (random-uuid)) "GitLab Spec"  gitlab-spec)]
    (testing "transformed spec should comply to AMOS format"
      (is (= true (m/validate amos/AMOS amos))))
    (testing "schemas can generate with defintions"
      (is (= true
             (vector?
              (try-gen-operations (amos/make-definitions-index amos)
                                  (:amos/operations amos))))))))

(comment
  (mg/generate
   [:map
    ["id" [:int]]
    ["name" [:string]]
    ["description" [:string]]])
  )

(deftest gitlab-v3-OAS2
  (let [gitlab-spec (-> "specifications/gitlab-v3.json"
                        (sys/open-api-from-file-path))
        amos        (sut/open-api-v2->amos
                     (fn [_] (random-uuid)) "GitLab Spec" gitlab-spec)]
    (testing "transformed spec should comply to AMOS format"
      (is (= true (m/validate amos/AMOS amos))))))


(comment
  (m/schema [:map])
  )
(comment
  (def feature-spec
    (-> "feature-service.json"
        (sys/open-api-from-file-path)))

  (def feature-amos (sut/open-api-v2->amos
                     (fn [_] (random-uuid)) "Feature Spec"  feature-spec))

  (def feature-definitions (amos/definitions-index feature-amos))

  ;;;;;;
  (def example-op
    {:operation/id #uuid "c790579b-f58f-4dac-b4a8-63a7b401c88a",
     :operation/info
     #:info{:name "getFeaturesForProduct",
            :key  :getFeaturesForProduct},
     :operation/parameters
     [{:data/schema [:map ["productName" :string]], :http/in :path}],
     :operation/responses
     [{:http/status 200, :data/schema [:set :definitions/Feature]}],
     :http/method  :get,
     :http/url     "/products/{productName}/features"})

  (mg/generate (:data/schema (first (:operation/responses example-op)))
               {:registry (merge (m/default-schemas)
                                 feature-definitions)
                :size 2})

  (try-gen feature-definitions :operation/responses :data/schema example-op)
  (try-gen feature-definitions :operation/parameters :data/schema example-op)
   (try-gen-operations feature-definitions (:amos/operations feature-amos))
  
  ;;;;;;
  (def gitlab-spec (-> "gitlab-v3.json"
                       (sys/open-api-from-file-path)))

  (def gitlab-amos (sut/open-api-v2->amos
                     (fn [_] (random-uuid)) "GitLab Spec" gitlab-spec))
  
  (defn humanize
    [schema value registry]
    (let [opt {:registry (merge (m/default-schemas) registry)}]
      (-> schema
          (m/explain value opt)
          (me/humanize {:wrap #(select-keys % [:value :message])}))))

  (humanize sut/OpenAPIV2Specification feature-spec {})
  (m/validate sut/OpenAPIV2Specification feature-spec)
  
  (repl/generate sut/OpenAPIV2Path {:size 2})

  (sut/open-api-v2->amos
   (fn [_] (random-uuid)) "Feature Spec"  feature-spec)
  
  (def feature-service-amos
    {:amos/id         #uuid "504e0ebe-22f0-4f0e-9613-37919e4ccc67"
     :amos/name       "Features service"
     :amos/data-specs []
     :amos/operations
     [;;;;
      ;; /products
      {:operation/id            #uuid "6365e4ed-b60a-4237-927d-6c69af62a4dd"
       :operation/name          "get:/products"
       :operation/key           :get-products
       :operation/response-spec [:map
                                 [:status [:enum 200]]
                                 [:body
                                  [:vector
                                   [:string]]]]}
    ;;;;
      ;; /products/{productName}
      {:operation/id             (java.util.UUID/randomUUID)
       :operation/name           "get:/products/{productName}"
       :operation/key            :get-product
       :operation/response-spec  [:map
                                  [:status [:enum 200]]
                                  ;; TODO - refine
                                  [:body 'Product]]
       :operation/parameter-spec [:map
                                  ["path"
                                   [:map ["productName" string?]]]]}
      {:operation/id             (java.util.UUID/randomUUID)
       :operation/name           "post:/products/{productName}"
       :operation/key            :post-product
       :operation/response-spec  [:map
                                  [:status [:enum 201]]]
       :operation/parameter-spec [:map
                                  ["path"
                                   ;; NOTE - It seems that this cannot be empty
                                   ;; the Spec do not say this, so it should be a finding
                                   ;; to report by the tooling
                                   [:map ["productName" string?]]]]}
      {:operation/id             (java.util.UUID/randomUUID)
       :operation/name           "delete:/products/{productName}"
       :operation/key            :delete-product
       :operation/response-spec  [:map
                                  [:status [:enum 204]]]
       :operation/parameter-spec [:map
                                  ["path"
                                   [:map ["productName" [:string]]]]]}

    ;;;;
      ;; /products/{productName}/configurations/{configurationName}
      ]}))
