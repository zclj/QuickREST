(ns unit.quickrest.alpha.resources.schema-test
  (:require [quickrest.alpha.resources.schema :as sut]
            [clojure.test :refer [deftest testing is]]
            [malli.generator :as mg]
            [malli.core :as m]
            [malli.util :as mu]
            [quickrest.alpha.resources.xtdb :as rxt]
            [quickrest.alpha.main :as main]))

;; There are different scenarios
;; 1 - Operation
(def example-op
  {:operation/id        #uuid "dbe558d0-64d9-4e00-8c4c-840ef9f35927",
   :operation/info
   #:info{:name "deleteFeature", :key :deleteFeature},
   :operation/parameters
   [{:data/schema [:map ["productName" :string]],
     :http/in     :path}
    {:data/schema [:map ["configurationName" :string]],
     :http/in     :path}
    {:data/schema [:map ["featureName" :string]],
     :http/in     :path}
    {:data/schema [:map ["productName" :string]],
     :http/in     :path}
    {:data/schema [:map ["configurationName" :string]],
     :http/in     :path}],
   :operation/responses [#:data{:schema [:map-of :int :any]}],
   :http/method         :delete,
   :http/url
   "/products/{productName}/configurations/{configurationName}/features/{featureName}"})
;; An operation do not have name on the map:s, since they are only parts
;; The name of the Aggregate map should be the parameter name

(deftest transform-operation
  (is (= {:operation/id   #uuid "dbe558d0-64d9-4e00-8c4c-840ef9f35927",          
          :operation/info #:info{:name "deleteFeature", :key :deleteFeature},
          :operation/parameters
          [{:data/schema [:map ["productName" :string]],
            :http/in     :path,
            :schema/nodes
            {"string"
             #:schema.node{:id         "string",
                           :type       :string,
                           :name       "Base type of string",
                           :components []},
             "map.entry/productName::string"
             #:schema.node{:id   "map.entry/productName::string",
                           :type :map/entry,
                           :name "productName",
                           :components
                           ["map.key/productName" "map.value/string"]},
             "map.key/productName"
             #:schema.node{:id         "map.key/productName",
                           :type       :map/key,
                           :name       "productName",
                           :components ["keyword"]},
             "map.value/string"
             #:schema.node{:id         "map.value/string",
                           :type       :map/value,
                           :name       "string",
                           :components ["string"]},
             "foo"
             #:schema.node{:id         "foo",
                           :type       :container/map,
                           :name       "foo",
                           :components ["map.entry/productName::string"],
                           :root       true}}}
           {:data/schema [:map ["configurationName" :string]],
            :http/in     :path,
            :schema/nodes
            {"string"
             #:schema.node{:id         "string",
                           :type       :string,
                           :name       "Base type of string",
                           :components []},
             "map.entry/configurationName::string"
             #:schema.node{:id   "map.entry/configurationName::string",
                           :type :map/entry,
                           :name "configurationName",
                           :components
                           ["map.key/configurationName" "map.value/string"]},
             "map.key/configurationName"
             #:schema.node{:id         "map.key/configurationName",
                           :type       :map/key,
                           :name       "configurationName",
                           :components ["keyword"]},
             "map.value/string"
             #:schema.node{:id         "map.value/string",
                           :type       :map/value,
                           :name       "string",
                           :components ["string"]},
             "foo"
             #:schema.node{:id         "foo",
                           :type       :container/map,
                           :name       "foo",
                           :components ["map.entry/configurationName::string"],
                           :root       true}}}
           {:data/schema [:map ["featureName" :string]],
            :http/in     :path,
            :schema/nodes
            {"string"
             #:schema.node{:id         "string",
                           :type       :string,
                           :name       "Base type of string",
                           :components []},
             "map.entry/featureName::string"
             #:schema.node{:id   "map.entry/featureName::string",
                           :type :map/entry,
                           :name "featureName",
                           :components
                           ["map.key/featureName" "map.value/string"]},
             "map.key/featureName"
             #:schema.node{:id         "map.key/featureName",
                           :type       :map/key,
                           :name       "featureName",
                           :components ["keyword"]},
             "map.value/string"
             #:schema.node{:id         "map.value/string",
                           :type       :map/value,
                           :name       "string",
                           :components ["string"]},
             "foo"
             #:schema.node{:id         "foo",
                           :type       :container/map,
                           :name       "foo",
                           :components ["map.entry/featureName::string"],
                           :root       true}}}
           {:data/schema [:map ["productName" :string]],
            :http/in     :path,
            :schema/nodes
            {"string"
             #:schema.node{:id         "string",
                           :type       :string,
                           :name       "Base type of string",
                           :components []},
             "map.entry/productName::string"
             #:schema.node{:id   "map.entry/productName::string",
                           :type :map/entry,
                           :name "productName",
                           :components
                           ["map.key/productName" "map.value/string"]},
             "map.key/productName"
             #:schema.node{:id         "map.key/productName",
                           :type       :map/key,
                           :name       "productName",
                           :components ["keyword"]},
             "map.value/string"
             #:schema.node{:id         "map.value/string",
                           :type       :map/value,
                           :name       "string",
                           :components ["string"]},
             "foo"
             #:schema.node{:id         "foo",
                           :type       :container/map,
                           :name       "foo",
                           :components ["map.entry/productName::string"],
                           :root       true}}}
           {:data/schema [:map ["configurationName" :string]],
            :http/in     :path,
            :schema/nodes
            {"string"
             #:schema.node{:id         "string",
                           :type       :string,
                           :name       "Base type of string",
                           :components []},
             "map.entry/configurationName::string"
             #:schema.node{:id   "map.entry/configurationName::string",
                           :type :map/entry,
                           :name "configurationName",
                           :components
                           ["map.key/configurationName" "map.value/string"]},
             "map.key/configurationName"
             #:schema.node{:id         "map.key/configurationName",
                           :type       :map/key,
                           :name       "configurationName",
                           :components ["keyword"]},
             "map.value/string"
             #:schema.node{:id         "map.value/string",
                           :type       :map/value,
                           :name       "string",
                           :components ["string"]},
             "foo"
             #:schema.node{:id         "foo",
                           :type       :container/map,
                           :name       "foo",
                           :components ["map.entry/configurationName::string"],
                           :root       true}}}],
          :operation/responses
          [{:data/schema [:map-of :int :any],
            :schema/nodes
            {"int"
             #:schema.node{:id         "int",
                           :type       :int,
                           :name       "Base type of integer",
                           :components []},
             "any"
             #:schema.node{:id         "any",
                           :type       :any,
                           :name       "Base type of any",
                           :components []},
             "map-of->int::any"
             #:schema.node{:id         "map-of->int::any",
                           :type       :container/map-of,
                           :name       "map-of",
                           :components ["int" "any"],
                           :root       true}}}],
          :http/method    :delete,
          :http/url
          "/products/{productName}/configurations/{configurationName}/features/{featureName}",
          :schema/nodes
          {"responses/deleteFeature"
           #:schema.node{:id         "responses/deleteFeature",
                         :type       :responses,
                         :name       "deleteFeature responses",
                         :components ["map-of->int::any"]},
           "parameters/deleteFeature"
           #:schema.node{:id         "parameters/deleteFeature",
                         :type       :parameters,
                         :name       "deleteFeature parameters",
                         :components ["foo" "foo" "foo" "foo" "foo"]},
           "operation/deleteFeature"
           #:schema.node{:id   "operation/deleteFeature",
                         :type :operation,
                         :name "deleteFeature",
                         :components
                         ["responses/deleteFeature"
                          "parameters/deleteFeature"]}}}
         (sut/transform-operation
          {:gen-name-f (fn [_] "foo")} example-op))))

(def example-op-with-ref
  {:operation/id #uuid "38f5eecc-3ed0-443b-8387-47f0ee7804aa",
   :operation/info
   #:info{:name "getConfigurationWithNameForProduct",
          :key  :getConfigurationWithNameForProduct},
   :operation/parameters
   [{:data/schema [:map ["productName" :string]],
     :http/in     :path}
    {:data/schema [:map ["configurationName" :string]],
     :http/in     :path}],
   :operation/responses
   [{:http/status 200,
     :data/schema [:ref :definitions/ProductConfiguration]}],
   :http/method  :get,
   :http/url
   "/products/{productName}/configurations/{configurationName}"})

(deftest transform-with-simple-refs
  (is (= {:operation/id         #uuid "38f5eecc-3ed0-443b-8387-47f0ee7804aa",
          :operation/info
          #:info{:name "getConfigurationWithNameForProduct",
                 :key  :getConfigurationWithNameForProduct},
          :operation/parameters [],
          :operation/responses
          [{:http/status 200,
            :data/schema [:ref :definitions/ProductConfiguration],
            :schema/nodes
            {"ref::definitions/ProductConfiguration"
             #:schema.node{:id         "ref::definitions/ProductConfiguration",
                           :type       :reference,
                           :name       "ref",
                           :components ["definitions/ProductConfiguration"],
                           :root       true}}}],
          :http/method          :get,
          :http/url
          "/products/{productName}/configurations/{configurationName}",
          :schema/nodes
          {"responses/getConfigurationWithNameForProduct"
           #:schema.node{:id         "responses/getConfigurationWithNameForProduct",
                         :type       :responses,
                         :name       "getConfigurationWithNameForProduct responses",
                         :components ["ref::definitions/ProductConfiguration"]},
           "parameters/getConfigurationWithNameForProduct"
           #:schema.node{:id         "parameters/getConfigurationWithNameForProduct",
                         :type       :parameters,
                         :name       "getConfigurationWithNameForProduct parameters",
                         :components []},
           "operation/getConfigurationWithNameForProduct"
           #:schema.node{:id   "operation/getConfigurationWithNameForProduct",
                         :type :operation,
                         :name "getConfigurationWithNameForProduct",
                         :components
                         ["responses/getConfigurationWithNameForProduct"
                          "parameters/getConfigurationWithNameForProduct"]}}}
         (sut/transform-operation
          {:gen-name-f (fn [_] "foo")
           :registry   (merge (m/default-schemas)
                              {:definitions/ProductConfiguration :any})}
          {:operation/id #uuid "38f5eecc-3ed0-443b-8387-47f0ee7804aa",
           :operation/info
           #:info{:name "getConfigurationWithNameForProduct",
                  :key  :getConfigurationWithNameForProduct},
           :operation/parameters
           [],
           :operation/responses
           [{:http/status 200,
             :data/schema [:ref :definitions/ProductConfiguration]}],
           :http/method  :get,
           :http/url
           "/products/{productName}/configurations/{configurationName}"}))))

(deftest transform-with-ref
  (is (= {:operation/id #uuid "38f5eecc-3ed0-443b-8387-47f0ee7804aa",
          :operation/info
          #:info{:name "getConfigurationWithNameForProduct",
                 :key  :getConfigurationWithNameForProduct},
          :operation/parameters
          [{:data/schema [:map ["productName" :string]],
            :http/in     :path,
            :schema/nodes
            {"string"
             #:schema.node{:id         "string",
                           :type       :string,
                           :name       "Base type of string",
                           :components []},
             "map.entry/productName::string"
             #:schema.node{:id   "map.entry/productName::string",
                           :type :map/entry,
                           :name "productName",
                           :components
                           ["map.key/productName" "map.value/string"]},
             "map.key/productName"
             #:schema.node{:id         "map.key/productName",
                           :type       :map/key,
                           :name       "productName",
                           :components ["keyword"]},
             "map.value/string"
             #:schema.node{:id         "map.value/string",
                           :type       :map/value,
                           :name       "string",
                           :components ["string"]},
             "foo"
             #:schema.node{:id         "foo",
                           :type       :container/map,
                           :name       "foo",
                           :components ["map.entry/productName::string"],
                           :root       true}}}
           {:data/schema [:map ["configurationName" :string]],
            :http/in     :path,
            :schema/nodes
            {"string"
             #:schema.node{:id         "string",
                           :type       :string,
                           :name       "Base type of string",
                           :components []},
             "map.entry/configurationName::string"
             #:schema.node{:id   "map.entry/configurationName::string",
                           :type :map/entry,
                           :name "configurationName",
                           :components
                           ["map.key/configurationName" "map.value/string"]},
             "map.key/configurationName"
             #:schema.node{:id         "map.key/configurationName",
                           :type       :map/key,
                           :name       "configurationName",
                           :components ["keyword"]},
             "map.value/string"
             #:schema.node{:id         "map.value/string",
                           :type       :map/value,
                           :name       "string",
                           :components ["string"]},
             "foo"
             #:schema.node{:id         "foo",
                           :type       :container/map,
                           :name       "foo",
                           :components ["map.entry/configurationName::string"],
                           :root       true}}}],
          :operation/responses
          [{:http/status 200,
            :data/schema [:ref :definitions/ProductConfiguration],
            :schema/nodes
            {"ref::definitions/ProductConfiguration"
             #:schema.node{:id         "ref::definitions/ProductConfiguration",
                           :type       :reference,
                           :name       "ref",
                           :components ["definitions/ProductConfiguration"],
                           :root       true}}}],
          :http/method  :get,
          :http/url
          "/products/{productName}/configurations/{configurationName}",
          :schema/nodes
          {"responses/getConfigurationWithNameForProduct"
           #:schema.node{:id         "responses/getConfigurationWithNameForProduct",
                         :type       :responses,
                         :name       "getConfigurationWithNameForProduct responses",
                         :components ["ref::definitions/ProductConfiguration"]},
           "parameters/getConfigurationWithNameForProduct"
           #:schema.node{:id         "parameters/getConfigurationWithNameForProduct",
                         :type       :parameters,
                         :name       "getConfigurationWithNameForProduct parameters",
                         :components ["foo" "foo"]},
           "operation/getConfigurationWithNameForProduct"
           #:schema.node{:id   "operation/getConfigurationWithNameForProduct",
                         :type :operation,
                         :name "getConfigurationWithNameForProduct",
                         :components
                         ["responses/getConfigurationWithNameForProduct"
                          "parameters/getConfigurationWithNameForProduct"]}}}
         (sut/transform-operation
          {:gen-name-f (fn [_] "foo")
           :registry   (merge (m/default-schemas)
                              {:definitions/ProductConfiguration :any})}
          example-op-with-ref))))

(deftest transform-operations
  (is (set? (sut/transform-operations
             {:gen-name-f (fn [_] "foo")}
             [example-op]))))


;; 2 - Definitions
(def example-def
  #:data{:schema
         [:map
          ["id" [:int]]
          ["name" [:string]]
          ["features" [:set :definitions/Feature]]
          ["constraints"
           [:set :definitions/FeatureConstraint]]],
         :name "Product",
         :key  :definitions/Product})
;; In this case the name is contained on the definition and the aggregate is complete

(deftest make-definitions-registry
  (is (= {:definitions/ProductsConfigurationFeaturesResource
          [:map]
          :definitions/Product
          [:map
           ["id" [:int]]
           ["name" [:string]]
           ["features" [:set :definitions/Feature]]
           ["constraints" [:set :definitions/FeatureConstraint]]]}
         (sut/make-definitions-registry
          #{#:data{:schema [:map],
                   :name   "ProductsConfigurationFeaturesResource",
                   :key
                   :definitions/ProductsConfigurationFeaturesResource}
            #:data{:schema
                   [:map
                    ["id" [:int]]
                    ["name" [:string]]
                    ["features" [:set :definitions/Feature]]
                    ["constraints"
                     [:set :definitions/FeatureConstraint]]],
                   :name "Product",
                   :key  :definitions/Product}})))
  )

(deftest transform-definition
  (testing "Basic definition"
    (is (= #:data
           {:schema
            [:map
             ["features" [:vector :definitions/Feature]]]
            :name "Product",
            :key  :definitions/Product
            :schema/nodes
            {"vector->definitions/Feature"
             #:schema.node{:id         "vector->definitions/Feature",
                           :type       :container/vector,
                           :name       "vector",
                           :components ["definitions/Feature"]},
             "map.entry/features::vector->definitions/Feature"
             #:schema.node{:id   "map.entry/features::vector->definitions/Feature",
                           :type :map/entry,
                           :name "features",
                           :components
                           ["map.key/features"
                            "map.value/vector->definitions/Feature"]},
             "map.key/features"
             #:schema.node{:id         "map.key/features",
                           :type       :map/key,
                           :name       "features",
                           :components ["keyword"]},
             "map.value/vector->definitions/Feature"
             #:schema.node{:id         "map.value/vector->definitions/Feature",
                           :type       :map/value,
                           :name       "vector->definitions/Feature",
                           :components ["vector->definitions/Feature"]},
             "definitions/Product"
             #:schema.node{:id   "definitions/Product",
                           :type :container/map,
                           :name "definitions/Product",
                           :components
                           ["map.entry/features::vector->definitions/Feature"],
                           :root true}}}
           (sut/transform-definition
            {:registry (merge (m/default-schemas)
                              {:definitions/Feature           :any
                               :definitions/FeatureConstraint :any})}
            #:data{:schema
                   [:map
                    ["features" [:vector :definitions/Feature]]]
                   :name "Product",
                   :key  :definitions/Product})))))

(deftest transform-definitions
  (is (= #{{:data/schema [:map ["id" [:int]] ["type" [:string]]],
            :data/name   "FeatureConstraint",
            :data/key    :definitions/FeatureConstraint,
            :schema/nodes
            {"int"
             #:schema.node{:id         "int",
                           :type       :int,
                           :name       "Base type of integer",
                           :components []},
             "map.key/id"
             #:schema.node{:id         "map.key/id",
                           :type       :map/key,
                           :name       "id",
                           :components ["keyword"]},
             "string"
             #:schema.node{:id         "string",
                           :type       :string,
                           :name       "Base type of string",
                           :components []},
             "map.value/string"
             #:schema.node{:id         "map.value/string",
                           :type       :map/value,
                           :name       "string",
                           :components ["string"]},
             "map.key/type"
             #:schema.node{:id         "map.key/type",
                           :type       :map/key,
                           :name       "type",
                           :components ["keyword"]},
             "map.value/int"
             #:schema.node{:id         "map.value/int",
                           :type       :map/value,
                           :name       "int",
                           :components ["int"]},
             "map.entry/type::string"
             #:schema.node{:id         "map.entry/type::string",
                           :type       :map/entry,
                           :name       "type",
                           :components ["map.key/type" "map.value/string"]},
             "definitions/FeatureConstraint"
             #:schema.node{:id   "definitions/FeatureConstraint",
                           :type :container/map,
                           :name "definitions/FeatureConstraint",
                           :components
                           ["map.entry/id::int" "map.entry/type::string"],
                           :root true},
             "map.entry/id::int"
             #:schema.node{:id         "map.entry/id::int",
                           :type       :map/entry,
                           :name       "id",
                           :components ["map.key/id" "map.value/int"]}}}
           {:data/schema
            [:map
             ["id" [:int]]
             ["name" [:string]]
             ["features" [:set :definitions/Feature]]
             ["constraints" [:set :definitions/FeatureConstraint]]],
            :data/name "Product",
            :data/key  :definitions/Product,
            :schema/nodes
            {"int"
             #:schema.node{:id         "int",
                           :type       :int,
                           :name       "Base type of integer",
                           :components []},
             "map.entry/constraints::set"
             #:schema.node{:id         "map.entry/constraints::set",
                           :type       :map/entry,
                           :name       "constraints",
                           :components ["map.key/constraints" "map.value/set"]},
             "map.key/id"
             #:schema.node{:id         "map.key/id",
                           :type       :map/key,
                           :name       "id",
                           :components ["keyword"]},
             "string"
             #:schema.node{:id         "string",
                           :type       :string,
                           :name       "Base type of string",
                           :components []},
             "map.key/constraints"
             #:schema.node{:id         "map.key/constraints",
                           :type       :map/key,
                           :name       "constraints",
                           :components ["keyword"]},
             "map.entry/features::set"
             #:schema.node{:id         "map.entry/features::set",
                           :type       :map/entry,
                           :name       "features",
                           :components ["map.key/features" "map.value/set"]},
             "map.value/set"
             #:schema.node{:id         "map.value/set",
                           :type       :map/value,
                           :name       "set",
                           :components ["set"]},
             "map.key/features"
             #:schema.node{:id         "map.key/features",
                           :type       :map/key,
                           :name       "features",
                           :components ["keyword"]},
             "map.value/string"
             #:schema.node{:id         "map.value/string",
                           :type       :map/value,
                           :name       "string",
                           :components ["string"]},
             "definitions/Product"
             #:schema.node{:id   "definitions/Product",
                           :type :container/map,
                           :name "definitions/Product",
                           :components
                           ["map.entry/id::int"
                            "map.entry/name::string"
                            "map.entry/features::set"
                            "map.entry/constraints::set"],
                           :root true},
             "map.value/int"
             #:schema.node{:id         "map.value/int",
                           :type       :map/value,
                           :name       "int",
                           :components ["int"]},
             "map.entry/name::string"
             #:schema.node{:id         "map.entry/name::string",
                           :type       :map/entry,
                           :name       "name",
                           :components ["map.key/name" "map.value/string"]},
             "map.key/name"
             #:schema.node{:id         "map.key/name",
                           :type       :map/key,
                           :name       "name",
                           :components ["keyword"]},
             "map.entry/id::int"
             #:schema.node{:id         "map.entry/id::int",
                           :type       :map/entry,
                           :name       "id",
                           :components ["map.key/id" "map.value/int"]}}}
           {:data/schema
            [:map ["id" [:int]] ["name" [:string]] ["description" [:string]]],
            :data/name "Feature",
            :data/key  :definitions/Feature,
            :schema/nodes
            {"int"
             #:schema.node{:id         "int",
                           :type       :int,
                           :name       "Base type of integer",
                           :components []},
             "map.key/id"
             #:schema.node{:id         "map.key/id",
                           :type       :map/key,
                           :name       "id",
                           :components ["keyword"]},
             "string"
             #:schema.node{:id         "string",
                           :type       :string,
                           :name       "Base type of string",
                           :components []},
             "map.value/string"
             #:schema.node{:id         "map.value/string",
                           :type       :map/value,
                           :name       "string",
                           :components ["string"]},
             "map.entry/description::string"
             #:schema.node{:id   "map.entry/description::string",
                           :type :map/entry,
                           :name "description",
                           :components
                           ["map.key/description" "map.value/string"]},
             "map.value/int"
             #:schema.node{:id         "map.value/int",
                           :type       :map/value,
                           :name       "int",
                           :components ["int"]},
             "map.entry/name::string"
             #:schema.node{:id         "map.entry/name::string",
                           :type       :map/entry,
                           :name       "name",
                           :components ["map.key/name" "map.value/string"]},
             "definitions/Feature"
             #:schema.node{:id   "definitions/Feature",
                           :type :container/map,
                           :name "definitions/Feature",
                           :components
                           ["map.entry/id::int"
                            "map.entry/name::string"
                            "map.entry/description::string"],
                           :root true},
             "map.key/description"
             #:schema.node{:id         "map.key/description",
                           :type       :map/key,
                           :name       "description",
                           :components ["keyword"]},
             "map.key/name"
             #:schema.node{:id         "map.key/name",
                           :type       :map/key,
                           :name       "name",
                           :components ["keyword"]},
             "map.entry/id::int"
             #:schema.node{:id         "map.entry/id::int",
                           :type       :map/entry,
                           :name       "id",
                           :components ["map.key/id" "map.value/int"]}}}
           {:data/schema [:map],
            :data/name   "ProductsConfigurationFeaturesResource",
            :data/key    :definitions/ProductsConfigurationFeaturesResource,
            :schema/nodes
            {"definitions/ProductsConfigurationFeaturesResource"
             #:schema.node{:id
                           "definitions/ProductsConfigurationFeaturesResource",
                           :type       :container/map,
                           :name
                           "definitions/ProductsConfigurationFeaturesResource",
                           :components [],
                           :root       true}}}}
         (sut/transform-definitions
          #{#:data{:schema [:map],
                   :name   "ProductsConfigurationFeaturesResource",
                   :key
                   :definitions/ProductsConfigurationFeaturesResource}
            #:data{:schema
                   [:map
                    ["id" [:int]]
                    ["name" [:string]]
                    ["features" [:set :definitions/Feature]]
                    ["constraints"
                     [:set :definitions/FeatureConstraint]]],
                   :name "Product",
                   :key  :definitions/Product}
            #:data{:schema
                   [:map
                    ["id" [:int]]
                    ["name" [:string]]
                    ["description" [:string]]],
                   :name "Feature",
                   :key  :definitions/Feature}
            #:data{:schema [:map ["id" [:int]] ["type" [:string]]],
                   :name   "FeatureConstraint",
                   :key    :definitions/FeatureConstraint}}))))

(comment
  (m/schema [:map])
  )


(comment
  (mg/generate (:data/schema example-def)
               {:registry (merge (m/default-schemas)
                                 {:definitions/Feature :any
                                  :definitions/FeatureConstraint :any})})

  (mu/update-properties [:map [:foo [:ref :bar]]]
                        (fn [p] p;;(assoc p :foo/bar :baz)
                          )
                        {:registry (merge (m/default-schemas)
                                          {:definitions/Feature :any
                                           :definitions/FeatureConstraint :any})}
                        )

  (mu/optional-keys [:map [:foo [:ref :definitions/Feature]]]
                    {:registry (merge (m/default-schemas)
                                          {:definitions/Feature :any
                                           :definitions/FeatureConstraint :any})})

  (mu/closed-schema [:map [:foo [:ref :definitions/Feature]]]
                  {:registry (merge (m/default-schemas)
                                          {:definitions/Feature :any
                                           :definitions/FeatureConstraint :any})})
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit

(deftest transform-schema
  (testing "simple map"
    (is (= {"string"
            {:schema.node/id         "string"
             :schema.node/type       :string
             :schema.node/name       "Base type of string"
             :schema.node/components []}
            "map.entry/name::string"
            {:schema.node/id         "map.entry/name::string"
             :schema.node/type       :map/entry
             :schema.node/name       "name"
             :schema.node/components ["map.key/name" "map.value/string"]}
            "map.key/name"
            {:schema.node/id         "map.key/name"
             :schema.node/type       :map/key
             :schema.node/name       "name"
             :schema.node/components ["keyword"]}
            "map.value/string"
            {:schema.node/id         "map.value/string"
             :schema.node/type       :map/value
             :schema.node/name       "string"
             :schema.node/components ["string"]}
            "noname"
            {:schema.node/id         "noname"
             :schema.node/type       :container/map
             :schema.node/name       "noname"
             :schema.node/root       true
             :schema.node/components ["map.entry/name::string"]}}
           (sut/transform
            {:gen-name-f (fn [_] "noname")}
            {}
            [:map
             ["name" [:string]]]))))
  (testing "map with multiple keys"
    (is (= {"map.entry/age::int"
            #:schema.node{:id         "map.entry/age::int",
                          :type       :map/entry,
                          :name       "age",
                          :components ["map.key/age" "map.value/int"]},
            "int"
            #:schema.node{:id         "int",
                          :type       :int,
                          :name       "Base type of integer",
                          :components []},
            "string"
            #:schema.node{:id         "string",
                          :type       :string,
                          :name       "Base type of string",
                          :components []},
            "map.value/string"
            #:schema.node{:id         "map.value/string",
                          :type       :map/value,
                          :name       "string",
                          :components ["string"]},
            "noname"
            #:schema.node{:id   "noname",
                          :type :container/map,
                          :name "noname",
                          :root true
                          :components
                          ["map.entry/name::string" "map.entry/age::int"]},
            "map.value/int"
            #:schema.node{:id         "map.value/int",
                          :type       :map/value,
                          :name       "int",
                          :components ["int"]},
            "map.entry/name::string"
            #:schema.node{:id         "map.entry/name::string",
                          :type       :map/entry,
                          :name       "name",
                          :components ["map.key/name" "map.value/string"]},
            "map.key/age"
            #:schema.node{:id         "map.key/age",
                          :type       :map/key,
                          :name       "age",
                          :components ["keyword"]},
            "map.key/name"
            #:schema.node{:id         "map.key/name",
                          :type       :map/key,
                          :name       "name",
                          :components ["keyword"]}}
           (sut/transform
            {:gen-name-f (fn [_] "noname")}
            {}
            [:map
             ["name" :string]
             ["age" :int]]))))
  (testing "vector"
    (is (= {"string"
            #:schema.node{:id         "string",
                          :type       :string,
                          :name       "Base type of string",
                          :components []},
            "vector->string"
            #:schema.node{:id         "vector->string",
                          :type       :container/vector,
                          :name       "vector",
                          :root       true,
                          :components ["string"]}}
           (sut/transform
            {:gen-name-f (fn [_] "noname")}
            {}
            [:vector [:string]]))))
  (testing "vector with ns"
    (is (= {"vector->foo/bar"          
            #:schema.node{:id         "vector->foo/bar",
                          :type       :container/vector,
                          :name       "vector",
                          :components ["foo/bar"],
                          :root       true}}
           (sut/transform
            {:gen-name-f (fn [_] "noname")
             :registry   (merge (m/default-schemas)
                                {:foo/bar :any})}
            {}
            [:vector :foo/bar]))))
  (testing "map-of"
    (is (= {"int"          
            #:schema.node{:id         "int",
                          :type       :int,
                          :name       "Base type of integer",
                          :components []},
            "any"
            #:schema.node{:id         "any",
                          :type       :any,
                          :name       "Base type of any",
                          :components []},
            "map-of->int::any"
            #:schema.node{:id         "map-of->int::any",
                          :type       :container/map-of,
                          :name       "map-of",
                          :root       true
                          :components ["int" "any"]}}
           (sut/transform
            {:gen-name-f (fn [_] "foo")} {} [:map-of :int :any]))))
  (testing "schema with properties"
    (is (= {"int"
            #:schema.node{:id         "int",
                          :type       :int,
                          :name       "Base type of integer",
                          :components []},
            "map.entry/bar::int"
            #:schema.node{:id         "map.entry/bar::int",
                          :type       :map/entry,
                          :name       "bar",
                          :components ["map.key/bar" "map.value/int"]},
            "map.key/bar"
            #:schema.node{:id         "map.key/bar",
                          :type       :map/key,
                          :name       "bar",
                          :components ["keyword"]},
            "map.value/int"
            #:schema.node{:id         "map.value/int",
                          :type       :map/value,
                          :name       "int",
                          :components ["int"]},
            "foo"
            #:schema.node{:id         "foo",
                          :type       :container/map,
                          :name       "foo",
                          :components ["map.entry/bar::int"],
                          :root       true}}
           (sut/transform
            {:gen-name-f (fn [_] "foo")}
            {} [:map ["bar" {:optional true} :int]])))))

