(ns unit.quickrest.alpha.engines.method.exploration-v2.meta-operation-generators-test
  (:require [quickrest.alpha.engines.method.exploration-v2.meta-operation-generators
             :as sut]
            [clojure.test :refer [deftest testing is]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [malli.generator :as mg]
            [clojure.test.check :as tc]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shrinking

;; Main point, DO NOT remove references of other operations

(comment
  (sut/shrinked-alternatives [1 2 3 4])

  (gen/vector)
  )

;;;
;; Shrinked alternatives

(deftest should-return-shrinked-alternatives-of-vector
  (testing "shrinking an empty vector is empty"
    (is (= [] (sut/shrinked-alternatives []))))
  (testing "shrinking a vector yields smaller alternatives"
    (is (= [[2 3] [1 3] [1 2]] (sut/shrinked-alternatives [1 2 3]))))
  (testing "shrinking a vector yields more smaller alternatives"
    (is (= [[2 3 4 5] [1 3 4 5] [1 2 4 5] [1 2 3 5] [1 2 3 4]]
           (sut/shrinked-alternatives [1 2 3 4 5]))))
  (testing "a vector of one value shrinks to empty list"
    (is (= [[]] (sut/shrinked-alternatives [1])))))

;;;;
;; Valid sequence

(deftest sequence-should-always-be-valid-with-no-parameter-references
  (testing "a sequence with no parameter references is valid"
    (is (sut/valid-reference-operation-sequence?
         [{:operation/id        #uuid "5eb796a4-0fa4-4655-8370-ba9a0ba58dfe",
           :operation/info
           #:info{:name "addExcludesConstraintToProduct",
                  :key  :addExcludesConstraintToProduct},
           :operation/parameters
           [{:data/schema     [:map ["productName" :string]],
             :parameter/name  "productName",
             :http/in         :path,
             :parameter/generation
             #:generation{:value {"productName" "rweuM8T"},
                          :id    #uuid "529c39ac-e37b-459a-85ef-f81485e2ad5e"},
             :parameter/value {"productName" "rweuM8T"}}
            {:data/schema     [:map ["sourceFeature" {:optional true} :string]],
             :parameter/name  "sourceFeature",
             :http/in         :form-data,
             :parameter/generation
             #:generation{:value {},
                          :id    #uuid "c3e350af-c744-4c62-8247-38113e5af972"},
             :parameter/value {}}
            {:data/schema
             [:map ["excludedFeature" {:optional true} :string]],
             :parameter/name  "excludedFeature",
             :http/in         :form-data,
             :parameter/generation
             #:generation{:value {},
                          :id    #uuid "eda6e1f5-7d9e-47eb-8143-373ecfb16dae"},
             :parameter/value {}}],
           :operation/responses [#:data{:schema [:map-of :int :any]}],
           :http/method         :post,
           :http/url            "/products/{productName}/constraints/excludes"}
          {:operation/id        #uuid "5eb796a4-0fa4-4655-8370-ba9a0ba58dfe",
           :operation/info
           #:info{:name "addExcludesConstraintToProduct",
                  :key  :addExcludesConstraintToProduct},
           :operation/parameters
           [{:data/schema     [:map ["productName" :string]],
             :parameter/name  "productName",
             :http/in         :path,
             :parameter/generation
             #:generation{:value {"productName" "rweuM8T"},
                          :id    #uuid "529c39ac-e37b-459a-85ef-f81485e2ad5e"},
             :parameter/value {"productName" "rweuM8T"}}
            {:data/schema     [:map ["sourceFeature" {:optional true} :string]],
             :parameter/name  "sourceFeature",
             :http/in         :form-data,
             :parameter/generation
             #:generation{:value {},
                          :id    #uuid "c3e350af-c744-4c62-8247-38113e5af972"},
             :parameter/value {}}
            {:data/schema
             [:map ["excludedFeature" {:optional true} :string]],
             :parameter/name  "excludedFeature",
             :http/in         :form-data,
             :parameter/generation
             #:generation{:value {},
                          :id    #uuid "eda6e1f5-7d9e-47eb-8143-373ecfb16dae"},
             :parameter/value {}}],
           :operation/responses [#:data{:schema [:map-of :int :any]}],
           :http/method         :post,
           :http/url            "/products/{productName}/constraints/excludes"}]))))

(deftest sequence-should-be-in-valid-with-missing-parameter-references
  (testing "a sequence with a missing parameter references is in-valid"
    (is (not
         (sut/valid-reference-operation-sequence?
          [{:operation/id        #uuid "5eb796a4-0fa4-4655-8370-ba9a0ba58dfe",
            :operation/info
            #:info{:name "addExcludesConstraintToProduct",
                   :key  :addExcludesConstraintToProduct},
            :operation/parameters
            [{:data/schema     [:map ["productName" :string]],
              :parameter/name  "productName",
              :http/in         :path,
              :parameter/generation
              #:generation{:value {"productName" "rweuM8T"},
                           ;; MISSING REFERENCE
                           :reference
                           #:reference{:operation-id
                                       #uuid "5eb796a4-0fa4-4655-8370-ba9a0ba58dfe",
                                       :generation-id
                                       #uuid "529c39ac-e37b-459a-85ef-f81485e2ad5e"},
                           :id    #uuid "85fd4529-4693-4726-985f-45b688bdb36c"},
              :parameter/value {"productName" "rweuM8T"}}
             {:data/schema     [:map ["sourceFeature" {:optional true} :string]],
              :parameter/name  "sourceFeature",
              :http/in         :form-data,
              :parameter/generation
              #:generation{:value {},
                           :reference
                           #:reference{:operation-id
                                       #uuid "5eb796a4-0fa4-4655-8370-ba9a0ba58dfe",
                                       :generation-id
                                       #uuid "c3e350af-c744-4c62-8247-38113e5af972"},
                           :id    #uuid "7a0b7478-2a24-44fb-b54b-d51f17d4d239"},
              :parameter/value {}}
             {:data/schema
              [:map ["excludedFeature" {:optional true} :string]],
              :parameter/name  "excludedFeature",
              :http/in         :form-data,
              :parameter/generation
              #:generation{:value {"productName" "rweuM8T"},
                           :reference
                           #:reference{:operation-id
                                       #uuid "5eb796a4-0fa4-4655-8370-ba9a0ba58dfe",
                                       :generation-id
                                       #uuid "529c39ac-e37b-459a-85ef-f81485e2ad5e"},
                           :id    #uuid "78ebc39f-2d41-491e-b26d-01cdda5f1f50"},
              :parameter/value {"productName" "rweuM8T"}}],
            :operation/responses [#:data{:schema [:map-of :int :any]}],
            :http/method         :post,
            :http/url            "/products/{productName}/constraints/excludes"}])))))

(deftest sequence-should-be-valid-with-included-parameter-references
  (testing "a sequence with parameter references is valid if the reference is in the sequence"
    (is (sut/valid-reference-operation-sequence?
         [{:operation/id        #uuid "5eb796a4-0fa4-4655-8370-ba9a0ba58dfe",
           :operation/info
           #:info{:name "addExcludesConstraintToProduct",
                  :key  :addExcludesConstraintToProduct},
           :operation/parameters
           [{:data/schema     [:map ["productName" :string]],
             :parameter/name  "productName",
             :http/in         :path,
             :parameter/generation
             #:generation{:value {"productName" "rweuM8T"},
                          :id    #uuid "529c39ac-e37b-459a-85ef-f81485e2ad5e"},
             :parameter/value {"productName" "rweuM8T"}}
            {:data/schema     [:map ["sourceFeature" {:optional true} :string]],
             :parameter/name  "sourceFeature",
             :http/in         :form-data,
             :parameter/generation
             #:generation{:value {},
                          :id    #uuid "c3e350af-c744-4c62-8247-38113e5af972"},
             :parameter/value {}}
            {:data/schema
             [:map ["excludedFeature" {:optional true} :string]],
             :parameter/name  "excludedFeature",
             :http/in         :form-data,
             :parameter/generation
             #:generation{:value {},
                          :id    #uuid "eda6e1f5-7d9e-47eb-8143-373ecfb16dae"},
             :parameter/value {}}],
           :operation/responses [#:data{:schema [:map-of :int :any]}],
           :http/method         :post,
           :http/url            "/products/{productName}/constraints/excludes"}
          {:operation/id        #uuid "5eb796a4-0fa4-4655-8370-ba9a0ba58dfe",
           :operation/info
           #:info{:name "addExcludesConstraintToProduct",
                  :key  :addExcludesConstraintToProduct},
           :operation/parameters
           [{:data/schema     [:map ["productName" :string]],
             :parameter/name  "productName",
             :http/in         :path,
             :parameter/generation
             #:generation{:value {"productName" "rweuM8T"},
                          :reference
                          #:reference{:operation-id
                                      #uuid "5eb796a4-0fa4-4655-8370-ba9a0ba58dfe",
                                      :generation-id
                                      #uuid "529c39ac-e37b-459a-85ef-f81485e2ad5e"},
                          :id    #uuid "85fd4529-4693-4726-985f-45b688bdb36c"},
             :parameter/value {"productName" "rweuM8T"}}
            {:data/schema     [:map ["sourceFeature" {:optional true} :string]],
             :parameter/name  "sourceFeature",
             :http/in         :form-data,
             :parameter/generation
             #:generation{:value {},
                          :reference
                          #:reference{:operation-id
                                      #uuid "5eb796a4-0fa4-4655-8370-ba9a0ba58dfe",
                                      :generation-id
                                      #uuid "c3e350af-c744-4c62-8247-38113e5af972"},
                          :id    #uuid "7a0b7478-2a24-44fb-b54b-d51f17d4d239"},
             :parameter/value {}}
            {:data/schema
             [:map ["excludedFeature" {:optional true} :string]],
             :parameter/name  "excludedFeature",
             :http/in         :form-data,
             :parameter/generation
             #:generation{:value {"productName" "rweuM8T"},
                          :reference
                          #:reference{:operation-id
                                      #uuid "5eb796a4-0fa4-4655-8370-ba9a0ba58dfe",
                                      :generation-id
                                      #uuid "529c39ac-e37b-459a-85ef-f81485e2ad5e"},
                          :id    #uuid "78ebc39f-2d41-491e-b26d-01cdda5f1f50"},
             :parameter/value {"productName" "rweuM8T"}}],
           :operation/responses [#:data{:schema [:map-of :int :any]}],
           :http/method         :post,
           :http/url            "/products/{productName}/constraints/excludes"}]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; More valid sequencing

(def example
  [{:operation/id
    #uuid "8d79e5fd-8936-40bf-a300-594e47a084a8",
    :operation/info
    #:info{:name "getAllProducts",
           :key  :getAllProducts},
    :operation/parameters [],
    :operation/responses
    [{:http/status 200,
      :data/schema [:vector [:string]],
      :schema/nodes
      {"string"
       #:schema.node{:id         "string",
                     :type       :string,
                     :name       "Base type of string",
                     :components []},
       "vector->string"
       #:schema.node{:id         "vector->string",
                     :type       :container/vector,
                     :name       "vector",
                     :components ["string"],
                     :root       true}}}],
    :http/method          :get,
    :http/url             "/products",
    :schema/nodes
    [{:schema.node/id         "responses/getAllProducts",
      :schema.node/type       :responses,
      :schema.node/name       "getAllProducts responses",
      :schema.node/components ["vector->string"],
      :xt/id                  "responses/getAllProducts"}
     {:schema.node/id         "parameters/getAllProducts",
      :schema.node/type       :parameters,
      :schema.node/name       "getAllProducts parameters",
      :schema.node/components [],
      :xt/id                  "parameters/getAllProducts"}
     {:schema.node/id   "operation/getAllProducts",
      :schema.node/type :operation,
      :schema.node/name "getAllProducts",
      :schema.node/components
      ["responses/getAllProducts"
       "parameters/getAllProducts"],
      :xt/id            "operation/getAllProducts"}],
    :xt/id                #uuid "8d79e5fd-8936-40bf-a300-594e47a084a8"}
   {:operation/id
    #uuid "d7c3602d-24b1-44cd-aa23-645a6cc26e51",
    :operation/info
    #:info{:name "deleteProductByName",
           :key  :deleteProductByName},
    :operation/parameters
    [{:data/schema     [:map ["productName" :string]],
      :parameter/name  "productName",
      :http/in         :path,
      :schema/nodes
      {"string"
       #:schema.node{:id         "string",
                     :type       :string,
                     :name       "Base type of string",
                     :components []},
       "map.entry/productName::string"
       #:schema.node{:id
                     "map.entry/productName::string",
                     :type :map/entry,
                     :name "productName",
                     :components
                     ["map.key/productName"
                      "map.value/string"]},
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
       #uuid "c16a915a-4867-4530-8e0d-dff2b1e5206d"
       #:schema.node{:id
                     #uuid "c16a915a-4867-4530-8e0d-dff2b1e5206d",
                     :type :container/map,
                     :name
                     #uuid "c16a915a-4867-4530-8e0d-dff2b1e5206d",
                     :components
                     ["map.entry/productName::string"],
                     :root true}},
      :parameter/generation
      #:generation{:value {"productName" "0"},
                   :id
                   #uuid "445a4ddd-b9b8-4030-bd31-092839947608"},
      :parameter/value {"productName" "0"}}],
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
    :http/method :delete,
    :http/url    "/products/{productName}",
    :schema/nodes
    ["responses/deleteProductByName"
     "parameters/deleteProductByName"
     "operation/deleteProductByName"],
    :xt/id       #uuid "d7c3602d-24b1-44cd-aa23-645a6cc26e51"}
   {:operation/id
    #uuid "8d79e5fd-8936-40bf-a300-594e47a084a8",
    :operation/info
    #:info{:name "getAllProducts",
           :key  :getAllProducts},
    :operation/parameters [],
    :operation/responses
    [{:http/status 200,
      :data/schema [:vector [:string]],
      :schema/nodes
      {"string"
       #:schema.node{:id         "string",
                     :type       :string,
                     :name       "Base type of string",
                     :components []},
       "vector->string"
       #:schema.node{:id         "vector->string",
                     :type       :container/vector,
                     :name       "vector",
                     :components ["string"],
                     :root       true}}}],
    :http/method          :get,
    :http/url             "/products",
    :schema/nodes
    [{:schema.node/id         "responses/getAllProducts",
      :schema.node/type       :responses,
      :schema.node/name       "getAllProducts responses",
      :schema.node/components ["vector->string"],
      :xt/id                  "responses/getAllProducts"}
     {:schema.node/id         "parameters/getAllProducts",
      :schema.node/type       :parameters,
      :schema.node/name       "getAllProducts parameters",
      :schema.node/components [],
      :xt/id                  "parameters/getAllProducts"}
     {:schema.node/id   "operation/getAllProducts",
      :schema.node/type :operation,
      :schema.node/name "getAllProducts",
      :schema.node/components
      ["responses/getAllProducts"
       "parameters/getAllProducts"],
      :xt/id            "operation/getAllProducts"}],
    :xt/id                #uuid "8d79e5fd-8936-40bf-a300-594e47a084a8"}
   ;; {:operation/id
   ;;  #uuid "d7c3602d-24b1-44cd-aa23-645a6cc26e51",
   ;;  :operation/info
   ;;  #:info{:name "deleteProductByName",
   ;;         :key  :deleteProductByName},
   ;;  :operation/parameters
   ;;  [{:data/schema     [:map ["productName" :string]],
   ;;    :parameter/name  "productName",
   ;;    :http/in         :path,
   ;;    :schema/nodes
   ;;    {"string"
   ;;     #:schema.node{:id         "string",
   ;;                   :type       :string,
   ;;                   :name       "Base type of string",
   ;;                   :components []},
   ;;     "map.entry/productName::string"
   ;;     #:schema.node{:id
   ;;                   "map.entry/productName::string",
   ;;                   :type :map/entry,
   ;;                   :name "productName",
   ;;                   :components
   ;;                   ["map.key/productName"
   ;;                    "map.value/string"]},
   ;;     "map.key/productName"
   ;;     #:schema.node{:id         "map.key/productName",
   ;;                   :type       :map/key,
   ;;                   :name       "productName",
   ;;                   :components ["keyword"]},
   ;;     "map.value/string"
   ;;     #:schema.node{:id         "map.value/string",
   ;;                   :type       :map/value,
   ;;                   :name       "string",
   ;;                   :components ["string"]},
   ;;     #uuid "c16a915a-4867-4530-8e0d-dff2b1e5206d"
   ;;     #:schema.node{:id
   ;;                   #uuid "c16a915a-4867-4530-8e0d-dff2b1e5206d",
   ;;                   :type :container/map,
   ;;                   :name
   ;;                   #uuid "c16a915a-4867-4530-8e0d-dff2b1e5206d",
   ;;                   :components
   ;;                   ["map.entry/productName::string"],
   ;;                   :root true}},
   ;;    :parameter/generation
   ;;    #:generation{:value {"productName" "0"},
   ;;                 :reference
   ;;                 #:reference{:operation-id
   ;;                             #uuid "d7c3602d-24b1-44cd-aa23-645a6cc26e51",
   ;;                             :generation-id
   ;;                             #uuid "445a4ddd-b9b8-4030-bd31-092839947608"},
   ;;                 :id
   ;;                 #uuid "a10e8baa-e70d-4e1e-83f9-481ca343cca1"},
   ;;    :parameter/value {"productName" "0"}}],
   ;;  :operation/responses
   ;;  [{:data/schema [:map-of :int :any],
   ;;    :schema/nodes
   ;;    {"int"
   ;;     #:schema.node{:id         "int",
   ;;                   :type       :int,
   ;;                   :name       "Base type of integer",
   ;;                   :components []},
   ;;     "any"
   ;;     #:schema.node{:id         "any",
   ;;                   :type       :any,
   ;;                   :name       "Base type of any",
   ;;                   :components []},
   ;;     "map-of->int::any"
   ;;     #:schema.node{:id         "map-of->int::any",
   ;;                   :type       :container/map-of,
   ;;                   :name       "map-of",
   ;;                   :components ["int" "any"],
   ;;                   :root       true}}}],
   ;;  :http/method :delete,
   ;;  :http/url    "/products/{productName}",
   ;;  :schema/nodes
   ;;  ["responses/deleteProductByName"
   ;;   "parameters/deleteProductByName"
   ;;   "operation/deleteProductByName"],
   ;;  :xt/id       #uuid "d7c3602d-24b1-44cd-aa23-645a6cc26e51"}
   {:operation/id
    #uuid "8d79e5fd-8936-40bf-a300-594e47a084a8",
    :operation/info
    #:info{:name "getAllProducts",
           :key  :getAllProducts},
    :operation/parameters [],
    :operation/responses
    [{:http/status 200,
      :data/schema [:vector [:string]],
      :schema/nodes
      {"string"
       #:schema.node{:id         "string",
                     :type       :string,
                     :name       "Base type of string",
                     :components []},
       "vector->string"
       #:schema.node{:id         "vector->string",
                     :type       :container/vector,
                     :name       "vector",
                     :components ["string"],
                     :root       true}}}],
    :http/method          :get,
    :http/url             "/products",
    :schema/nodes
    [{:schema.node/id         "responses/getAllProducts",
      :schema.node/type       :responses,
      :schema.node/name       "getAllProducts responses",
      :schema.node/components ["vector->string"],
      :xt/id                  "responses/getAllProducts"}
     {:schema.node/id         "parameters/getAllProducts",
      :schema.node/type       :parameters,
      :schema.node/name       "getAllProducts parameters",
      :schema.node/components [],
      :xt/id                  "parameters/getAllProducts"}
     {:schema.node/id   "operation/getAllProducts",
      :schema.node/type :operation,
      :schema.node/name "getAllProducts",
      :schema.node/components
      ["responses/getAllProducts"
       "parameters/getAllProducts"],
      :xt/id            "operation/getAllProducts"}],
    :xt/id                #uuid "8d79e5fd-8936-40bf-a300-594e47a084a8"}
   {:operation/id
    #uuid "3ee47b68-ca72-4309-9346-cb824de9e492",
    :operation/info
    #:info{:name "addProduct", :key :addProduct},
    :operation/parameters
    [{:data/schema     [:map ["productName" :string]],
      :parameter/name  "productName",
      :http/in         :path,
      :schema/nodes
      {"string"
       #:schema.node{:id         "string",
                     :type       :string,
                     :name       "Base type of string",
                     :components []},
       "map.entry/productName::string"
       #:schema.node{:id
                     "map.entry/productName::string",
                     :type :map/entry,
                     :name "productName",
                     :components
                     ["map.key/productName"
                      "map.value/string"]},
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
       #uuid "e970de22-76be-4e31-a625-229ca1f197b7"
       #:schema.node{:id
                     #uuid "e970de22-76be-4e31-a625-229ca1f197b7",
                     :type :container/map,
                     :name
                     #uuid "e970de22-76be-4e31-a625-229ca1f197b7",
                     :components
                     ["map.entry/productName::string"],
                     :root true}},
      :parameter/generation
      #:generation{:value {"productName" "0"},
                   :reference
                   #:reference{:operation-id
                               #uuid "d7c3602d-24b1-44cd-aa23-645a6cc26e51",
                               :generation-id
                               #uuid "445a4ddd-b9b8-4030-bd31-092839947608"},
                   :id
                   #uuid "3fa17411-e1ec-4ebd-8ef3-6864abf9beb5"},
      :parameter/value {"productName" "0"}}],
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
    :http/method :post,
    :http/url    "/products/{productName}",
    :schema/nodes
    ["responses/addProduct"
     "parameters/addProduct"
     "operation/addProduct"],
    :xt/id       #uuid "3ee47b68-ca72-4309-9346-cb824de9e492"}
   {:operation/id
    #uuid "8d79e5fd-8936-40bf-a300-594e47a084a8",
    :operation/info
    #:info{:name "getAllProducts",
           :key  :getAllProducts},
    :operation/parameters [],
    :operation/responses
    [{:http/status 200,
      :data/schema [:vector [:string]],
      :schema/nodes
      {"string"
       #:schema.node{:id         "string",
                     :type       :string,
                     :name       "Base type of string",
                     :components []},
       "vector->string"
       #:schema.node{:id         "vector->string",
                     :type       :container/vector,
                     :name       "vector",
                     :components ["string"],
                     :root       true}}}],
    :http/method          :get,
    :http/url             "/products",
    :schema/nodes
    [{:schema.node/id         "responses/getAllProducts",
      :schema.node/type       :responses,
      :schema.node/name       "getAllProducts responses",
      :schema.node/components ["vector->string"],
      :xt/id                  "responses/getAllProducts"}
     {:schema.node/id         "parameters/getAllProducts",
      :schema.node/type       :parameters,
      :schema.node/name       "getAllProducts parameters",
      :schema.node/components [],
      :xt/id                  "parameters/getAllProducts"}
     {:schema.node/id   "operation/getAllProducts",
      :schema.node/type :operation,
      :schema.node/name "getAllProducts",
      :schema.node/components
      ["responses/getAllProducts"
       "parameters/getAllProducts"],
      :xt/id            "operation/getAllProducts"}],
    :xt/id                #uuid "8d79e5fd-8936-40bf-a300-594e47a084a8"}
   {:operation/id
    #uuid "d7c3602d-24b1-44cd-aa23-645a6cc26e51",
    :operation/info
    #:info{:name "deleteProductByName",
           :key  :deleteProductByName},
    :operation/parameters
    [{:data/schema     [:map ["productName" :string]],
      :parameter/name  "productName",
      :http/in         :path,
      :schema/nodes
      {"string"
       #:schema.node{:id         "string",
                     :type       :string,
                     :name       "Base type of string",
                     :components []},
       "map.entry/productName::string"
       #:schema.node{:id
                     "map.entry/productName::string",
                     :type :map/entry,
                     :name "productName",
                     :components
                     ["map.key/productName"
                      "map.value/string"]},
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
       #uuid "c16a915a-4867-4530-8e0d-dff2b1e5206d"
       #:schema.node{:id
                     #uuid "c16a915a-4867-4530-8e0d-dff2b1e5206d",
                     :type :container/map,
                     :name
                     #uuid "c16a915a-4867-4530-8e0d-dff2b1e5206d",
                     :components
                     ["map.entry/productName::string"],
                     :root true}},
      :parameter/generation
      #:generation{:value {"productName" "0"},
                   :reference
                   #:reference{:operation-id
                               #uuid "d7c3602d-24b1-44cd-aa23-645a6cc26e51",
                               :generation-id
                               #uuid "445a4ddd-b9b8-4030-bd31-092839947608"},
                   :id
                   #uuid "e48351e4-f33a-4867-a4ce-0f9635f4387d"},
      :parameter/value {"productName" "0"}}],
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
    :http/method :delete,
    :http/url    "/products/{productName}",
    :schema/nodes
    ["responses/deleteProductByName"
     "parameters/deleteProductByName"
     "operation/deleteProductByName"],
    :xt/id       #uuid "d7c3602d-24b1-44cd-aa23-645a6cc26e51"}
   {:operation/id
    #uuid "8d79e5fd-8936-40bf-a300-594e47a084a8",
    :operation/info
    #:info{:name "getAllProducts",
           :key  :getAllProducts},
    :operation/parameters [],
    :operation/responses
    [{:http/status 200,
      :data/schema [:vector [:string]],
      :schema/nodes
      {"string"
       #:schema.node{:id         "string",
                     :type       :string,
                     :name       "Base type of string",
                     :components []},
       "vector->string"
       #:schema.node{:id         "vector->string",
                     :type       :container/vector,
                     :name       "vector",
                     :components ["string"],
                     :root       true}}}],
    :http/method          :get,
    :http/url             "/products",
    :schema/nodes
    [{:schema.node/id         "responses/getAllProducts",
      :schema.node/type       :responses,
      :schema.node/name       "getAllProducts responses",
      :schema.node/components ["vector->string"],
      :xt/id                  "responses/getAllProducts"}
     {:schema.node/id         "parameters/getAllProducts",
      :schema.node/type       :parameters,
      :schema.node/name       "getAllProducts parameters",
      :schema.node/components [],
      :xt/id                  "parameters/getAllProducts"}
     {:schema.node/id   "operation/getAllProducts",
      :schema.node/type :operation,
      :schema.node/name "getAllProducts",
      :schema.node/components
      ["responses/getAllProducts"
       "parameters/getAllProducts"],
      :xt/id            "operation/getAllProducts"}],
    :xt/id
    #uuid "8d79e5fd-8936-40bf-a300-594e47a084a8"}])

(deftest more-validation
  (is (sut/valid-reference-operation-sequence?
       example))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try out property stuff

(def example-operation
  {:operation/id        #uuid "5eb796a4-0fa4-4655-8370-ba9a0ba58dfe",
   :operation/info
   #:info{:name "addExcludesConstraintToProduct",
          :key  :addExcludesConstraintToProduct},
   :operation/parameters
   [{:data/schema    [:map ["productName" :string]],
     :parameter/name "productName",
     :http/in        :path}
    {:data/schema
     [:map ["sourceFeature" {:optional true} :string]],
     :parameter/name "sourceFeature",
     :http/in        :form-data}
    {:data/schema
     [:map ["excludedFeature" {:optional true} :string]],
     :parameter/name "excludedFeature",
     :http/in        :form-data}],
   :operation/responses [#:data{:schema [:map-of :int :any]}],
   :http/method         :post,
   :http/url            "/products/{productName}/constraints/excludes"})

(def example-operations
  [example-operation])

(comment
  (def dummy
    (prop/for-all [op-seq (sut/sized-meta-operation-sequence
                           ;; operations should also be an injected generator
                           {:operations example-operations
                            :parameter-generator
                            (fn [operation operation-sequence parameter]
                              ;;(random-parameter mg/generator parameter)
                              (sut/reference-param
                               ;; look these up for the given parameter
                               ["map.entry/productName::string"
                                "map.entry/sourceFeature::string"
                                "vector->string"]
                               operation-sequence
                               mg/generator
                               parameter))}
                           []
                           40
                           sut/shrink-operation-sequence)]
      (let [params   (mapcat :operation/parameters op-seq)
            p-1s     (filter #(= "productName" (:parameter/name %)) params)
            with-p-1 (filter #(or (= {"productName" ""} (:parameter/value %))
                                  (nil?
                                   (:generation/reference
                                    (:parameter/generation %)))) p-1s)]
        (println (str "with-p-1 " (into [] p-1s)))
        (if (seq with-p-1)
          true
          false))))

  (tc/quick-check 100 dummy)
  )
