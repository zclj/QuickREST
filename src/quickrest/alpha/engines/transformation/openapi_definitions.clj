(ns quickrest.alpha.engines.transformation.openapi-definitions
  (:require [malli.core :as m]
            [malli.util :as mu]
            [clojure.string :as s]
            [quickrest.alpha.resources.amos :as amos]
            [quickrest.alpha.resources.http :as http]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This transformation defines how to perform a transformation from an
;; OpenAPI Specification to an AMOS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Schemas

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OpenAPI

(def OpenAPIV2InfoObject
  [:map
   ["title" {:optional true} string?]
   ["description" {:optional true} string?]
   ["version" string?]])

(def OpenAPIV2MetaData
  [:map
   ["swagger" [:enum "2.0"]]
   ["basePath" {:optional true} :string]
   ["host" {:optional true} :string]
   ["info" OpenAPIV2InfoObject]
   ["schemes" {:optional true} [:vector [:enum "http" "https"]]]])

(def AlphanumericString [:re "[/a-zA-Z0-9]+"])

(def Formats
  ;; NOTE: date-time is according to spec, but dateTime used in practice
  [:enum "int32" "int64" "float" "double" "byte" "binary" "date" "date-time" "dateTime" "password"])

(def ArrayType
  [:enum "array"])

(def BaseTypes
  [:enum "string" "number" "integer" "boolean"])

(def FileType
  [:enum "file"])

(def FileItem
  [:map
   ["type" FileType]])

(def DefaultValue [:or :string :int :boolean])

(def BaseType
  [:map
   ["type" BaseTypes]])

(def SchemaRef
  [:map
   ["schema"
    [:map {:closed true}
     ["$ref" [:and {:gen/fmap '(partial str "#/definitions/")} AlphanumericString]]]]])

(def SchemaNodeObject
  [:map
    ["schema"
     [:map
      ["type" [:enum "object"]]
      ["additionalProperties"
       [:map
        ["type" {:optional true} [:enum "object"]]
        ["$ref" {:optional true}
         [:and {:gen/fmap '(partial str "#/definitions/")} AlphanumericString] ]]]]]])

(def SchemaNodeString
  [:map
   ["schema"
    [:map
     ["type" [:enum "string"]]]]])

(def ItemRequired
  [:map
   ["in" [:enum "body" "query" "path" "formData"]]
   ["name" [:re "[a-zA-Z0-9]+"]]])

(def ItemOptional
  [:map
   ["description" {:optional true} AlphanumericString]
   ;; TODO : default value must match the type
   ["default" {:optional true} DefaultValue]
   ["enum" {:optional true} [:vector [:or AlphanumericString :int]]]
   ["required" {:optional true} [:or boolean? [:vector :string]]]
   ;; TODO : should follow type?
   ["format" {:optional true} Formats]
   ["allowEmptyValue" {:optional true} boolean?]])

(def Item
  (mu/merge ItemRequired ItemOptional))

(def NoneEmptySequence
  [:fn (fn [m] (seq m))])

(def ArrayItemsObject
  ;; TODO - this can :or be array or other type
  ;; https://swagger.io/specification/v2/#itemsObject
  [:or
   [:map {:closed true}
    ["type" [:enum "string" "number" "integer" "boolean"]]
    ["format" {:optional true} Formats]
    ["default" {:optional true} DefaultValue]]
   [:map {:closed true}
    ["$ref" [:and {:gen/fmap '(partial str "#/definitions/")} AlphanumericString]]]
   [:map {:closed true}
    ["properties" map?]
    ["type" [:enum "object"]]]
   [:map-of :string :any]])

(def ArrayItem
  [:map
   ["type" [:enum "array"]]
   ["items" ArrayItemsObject]])

(def OpenAPIV2Parameter
  ;;[:map-of :string]
  [:or
   ;;(mu/merge Item array-item-spec)
   ;;(mu/merge Item body-item-spec)
   (mu/merge Item FileItem)
   (mu/merge Item ArrayItem)
   (mu/merge Item SchemaRef)
   (mu/merge Item SchemaNodeString)
   (mu/merge Item SchemaNodeObject)
   (mu/merge Item BaseType)
   [:map [:any]]])

(def Property
  [:map-of
   [:string {:min 1}]
   ;; TODO - do not know why this does not work in registry for instrument..
   ;;[:ref :json/schema]
   map?])

(def Properties
  [:map
   ;; not all schemas are well formed..
   ["properties" {:optional true} Property]
   ["type" {:optional true} [:enum "object"]]])

(def Reference
  [:map {:closed true}
   ["$ref" [:and {:gen/fmap '(partial str "#/definitions/")} AlphanumericString]]])

(def SchemaObject
  [:and
   [:or
    Reference
    ArrayItem
    Properties
    BaseType]
   ItemOptional
   NoneEmptySequence])

(def HeaderObject
  [:map-of :string
   (let [req-and-opt (mu/merge BaseType ItemOptional)]
    [:or
     (mu/merge req-and-opt ArrayItem)
     (mu/merge req-and-opt BaseType)])])

(def ExampleObject
  map?)

(def OpenAPIV2Response
  [:and
   [:map {:closed true}
    ["description" AlphanumericString]
    ["schema" {:optional true} SchemaObject]
    ["headers" {:optional true} HeaderObject]
    ["examples" {:optional true} ExampleObject]]
   NoneEmptySequence])

(def OpenAPIV2Responses
  [:and
   [:map
    ["default" {:optional true} OpenAPIV2Response]
    ["200" {:optional true} OpenAPIV2Response]
    ["201" {:optional true} OpenAPIV2Response]
    ["401" {:optional true} OpenAPIV2Response]
    ["403" {:optional true} OpenAPIV2Response]
    ["404" {:optional true} OpenAPIV2Response]
    ["500" {:optional true} OpenAPIV2Response]]
   NoneEmptySequence])

(def OpenAPIV2ResponsesEntry
  [:map
   ["responses" OpenAPIV2Responses]])

(def OpenAPIV2Parameters
  [:map
   ["parameters" [:vector OpenAPIV2Parameter]]])

(def OpenAPIV2OperationId
  [:map
   ["operationId" {:optional true} AlphanumericString]])

(def OpenAPIV2PathMethodEntry
  [:map
   ["operationId" {:optional true} AlphanumericString]
   ["summary" {:optional true} [:string {:min 1}]]
   ["description" {:optional true} [:string]]
   ["produces" {:optional true} [:vector {:min 1 :max 10}
                                 [:enum "application/json" "text/csv"
                                  "application/octet-stream"
                                  "application/vnd.tsdes.news+json;charset=UTF-8;version=2"
                                  "application/json;charset=UTF-8"
                                  "text/event-stream"
                                  ;; no idea what this means, but seen in the wild..
                                  "*/*"]]]
   ["parameters" {:optional true} [:vector OpenAPIV2Parameter]]
   ["tags" {:optional true} [:vector AlphanumericString]]
   ["responses" OpenAPIV2Responses]])

(def OpenAPIV2PathMethodKey [:enum "get" "delete" "put" "patch" "post" "head" "options"])

(def OpenAPIV2PathMethods
  [:map-of
   OpenAPIV2PathMethodKey
   OpenAPIV2PathMethodEntry])

(def OpenAPIV2Path
  [:map-of
   AlphanumericString
   OpenAPIV2PathMethods])

(def ArrayItemReference
  [:map
   ["type" [:enum "array"]]
   ["items" [:map ["$ref" [:string {:min 1}]]]]])

;; TODO: redundant?
(def OpenAPIV2JSONDefinition
  [:map {:closed true}
   ["properties" {:optional true}
     [:and
      [:map-of [:string {:min 1}]
       [:or
        (mu/closed-schema
         (mu/merge
          BaseType
          [:map
           ["format" {:optional true} Formats]
           ["readOnly" {:optional true} boolean?]
           ["enum" {:optional true} [:vector :string]]
           ["default" {:optional true} DefaultValue]
           ["description" {:optional true} string?]]))
        [:map
         ["type" [:enum "object"]]]
        ArrayItemReference
        ;; [:map
        ;;  ["$ref" [:string {:min 1}]]]
        Reference
        ]
       ;;(mu/closed-schema base-item-spec)
       ]
      NoneEmptySequence]]
    ["type" [:enum "object"]]
   ["description" {:optional true} string?]])

(def OpenAPIV2JSONDefinitions
  [:map-of
   [:string {:min 1}]
   SchemaObject
   ;;OpenAPIV2JSONDefinition
   ])

(def OpenAPIV2Specification
  (mu/merge
   OpenAPIV2MetaData
   [:map
    ["paths" OpenAPIV2Path]
    ;; An object to hold data types produced and consumed by operations.
    ["definitions" {:optional true} OpenAPIV2JSONDefinitions]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API

(defn open-api-parameter-type->AMOS-type
  {:malli/schema [:=> [:cat [:or BaseTypes ArrayType FileType]] :keyword]}
  [type-str]
  (cond
    (= type-str "string")  :string
    (= type-str "integer") :int
    (= type-str "number") :double
    (= type-str "boolean") :boolean
    :else :any))

(defn open-api-v2-meta-data->amos
  {:malli/schema [:=> [:cat OpenAPIV2MetaData] amos/AMOSDomainMetaData]}
  [open-api-v2-document]
  (amos/make-amos-domain-meta-data open-api-v2-document))

(defn open-api-v2-operation-id->amos
  {:malli/schema
   [:=>
    [:cat [:string {:min 1}] OpenAPIV2PathMethodKey OpenAPIV2OperationId]
    amos/AMOSOperationInfoEntry]}
  [url method {:strs [operationId produces]}]
  (if operationId
    {:info/name     operationId
     :info/key      (keyword operationId)
     :info/produces produces}
    {:info/name     (str method ":" url)
     :info/key      (keyword (str method url))
     :info/produces produces}))

(defn open-api-parameter-in->http-edn
  {:malli/schema [:=> [:cat http/HTTPInStr] http/HTTPInKey]}
  [in-str]
  (condp = in-str
    "path"     :path
    "formData" :form-data
    "query"    :query
    "body"     :body
    "head"     :head
    "options"  :options))

(defn make-http-parameter-in
  {:malli/schema [:=> [:cat http/HTTPInKey] http/HTTPIn]}
  [in]
  {:http/in in})

(defn make-http-url
  {:malli/schema [:=> [:cat [:string]] http/HTTPURL]}
  [url]
  {:http/url url})

(defn open-api-method->http-edn
  {:malli/schema [:=> [:cat http/HTTPMethodStr] http/HTTPMethodKey]}
  [method-str]
  (keyword method-str))

(defn make-http-method
  {:malli/schema [:=> [:cat http/HTTPMethodKey] http/HTTPMethod]}
  [method]
  {:http/method method})

(defn open-api-v2-parameter->amos
  {:malli/schema [:=> [:cat OpenAPIV2Parameter] amos/AMOSOperationParameter]}
  [{:strs [name in required type]}]
  (let [amos-type (open-api-parameter-type->AMOS-type type)
        http-in   (open-api-parameter-in->http-edn in)]
    (merge
     (amos/make-amos-operation-parameter name required amos-type)
     (make-http-parameter-in http-in))))

(defn open-api-v2-parameters->amos
  {:malli/schema [:=> [:cat :map] amos/AMOSOperationParametersSeq]}
  [{:strs [parameters]}]
  (mapv open-api-v2-parameter->amos parameters))

(defn schema-ref-to-spec-name
  [schema-object]
  (let [ref-path (get-in schema-object ["schema" "$ref"])
        ref-name (last (s/split ref-path #"/"))]
    (keyword "definitions" ref-name)))

(defn property-array-with-ref->definition-reference
  {:malli/schema [:=> [:cat ArrayItemReference] amos/AMOSSchema]}
  [property-array]
  (let [ref-path   (get-in property-array ["items" "$ref"])
        ref-name   (last (s/split ref-path #"/"))
        def-name   (keyword "definitions" ref-name)
        def-schema (if (get property-array "uniqueItems")
                     [:vector {:json-schema/unique-items true} def-name]
                     [:vector def-name])]
    def-schema))

(declare json-property->schema)

(defn json-array->schema
  [{:strs [type items] :as array}]
  (let [schema
        (cond
          (m/validate [:map
                       ["type" [:enum "array"]]
                       ["items" [:map ["type" [:enum "string"]]]]]
                      array)
          [:vector [:string]]

          (m/validate [:map
                       ["type" [:enum "array"]]
                       ["items" [:map
                                 ["type" [:enum "number"]]
                                 ["format" [:enum "double"]]]]]
                      array)
          [:vector [:double]]

          (m/validate [:map
                       ["type" [:enum "array"]]
                       ["items" [:map
                                 ["type" {:optional true} [:enum "integer"]]
                                 ["format" [:enum "int64"]]]]]
                      array)
          [:vector [:int]]

          (m/validate [:map
                       ["type" [:enum "array"]]
                       ["items" [:map ["type" [:enum "object"]]]]]
                      array)
          (conj
           [:vector]
           (into
            [:map]
            (map (fn [[k v]] [k (json-property->schema v)])
                 (get-in array ["items" "properties"])))))]
    ;;(println (str "Type " type " - items " items " - schema " schema))
    schema))

(def ObjectSchema
  [:map
   ["type" [:enum "object"]]
   ["properties" [:map-of :string :map]]])

(def ObjectWOProperties
  [:map
   ["type" [:enum "object"]]])

(defn json-property->schema
  {:malli/schema [:=>
                  [:cat [:or BaseType ArrayItemReference Reference ArrayItem SchemaRef
                         SchemaNodeObject SchemaNodeString ObjectWOProperties]]
                  amos/AMOSSchema]}
  [{:strs [type _format] :as property}]
  (cond
    (= type "integer") [:int]
    (= type "string")  [:string]
    (= type "boolean") [:boolean]
    (m/validate ArrayItemReference property)
    (property-array-with-ref->definition-reference property)
    (m/validate Reference property)
    [:ref (schema-ref-to-spec-name {"schema" property})]
    (m/validate ArrayItem property)
    (json-array->schema property)
    (m/validate ObjectSchema property)
    (into
     [:map]
     (map (fn [[k v]] [k (json-property->schema v)]) (get property "properties")))
    :else              [:any]))

(comment
  (json-property->schema
   {"type" "object"
    "properties"
    {"name" {"type" "string"}
     "matches" {"type" "array"
                "items" {"type" "object"
                         "properties" {"message" {"type" "string"}}}}}})
  )

(defn json-object-properties->schema
  {:malli/schema [:=> [:cat Properties] amos/AMOSSchemaMapEntries]}
  [{:strs [properties]}]
  (map (fn [[k v]] [k (json-property->schema v)]) properties))

(defn json-object->schema
  {:malli/schema [:=> [:cat Properties] amos/AMOSSchemaMap]}
  [definition]
  (into [:map] (json-object-properties->schema definition)))

(defn json-schema->schema
  [{:strs [schema]}]
  (cond
    (get schema "type") (json-property->schema schema)
    (get schema "properties") (json-object->schema schema)
    (get schema "$ref") (json-property->schema schema)
    :else :any))

(comment
  (json-schema->schema
   {"schema"
    {"properties"
     {"software"
      {"type" "object"
       "properties"
       {"name" {"type" "string"}}}}}})
  )
(defn open-api-v2-response->amos
  {:malli/schema [:=> [:cat OpenAPIV2Responses] amos/AMOSOperationResponse]}
  [response]
  (let [response-k (first (keys response))
        response-v (first (vals response))]
    (cond
      (= response-k "default") {:data/schema [:map-of :int :any]}
      :else                    {:http/status (parse-long response-k)
                                :data/schema (json-schema->schema response-v)})))

(defn open-api-v2-responses->amos
  {:malli/schema [:=> [:cat OpenAPIV2ResponsesEntry] amos/AMOSOperationResponsesSeq]}
  [{:strs [responses]}]
  (reduce-kv
   (fn [acc k v]
     (conj acc (open-api-v2-response->amos {k v})))
   []
   responses))

(defn open-api-v2-path-entry->amos
  {:malli/schema
   [:=>
    [:cat [:string {:min 1}] amos/OperationId OpenAPIV2PathMethodKey OpenAPIV2PathMethodEntry]
    amos/AMOSOperation]}
  [url id method path]
  (let [amos-parameters (open-api-v2-parameters->amos path)
        amos-responses  (open-api-v2-responses->amos path)
        amos-info       (open-api-v2-operation-id->amos url method path)]
    (merge
     (amos/make-amos-operation-id id)
     (amos/make-amos-operation-info amos-info)
     (amos/make-amos-operation-parameters amos-parameters)
     (amos/make-amos-operation-responses amos-responses))))

(defn open-api-v2-path->amos
  {:malli/schema
   [:=>
    [:cat [:string {:min 1}]
     amos/OperationId OpenAPIV2PathMethodKey OpenAPIV2PathMethodEntry]
    amos/AMOSOperation]}
  [url id path-k path-v]
  (let [path-entry  (open-api-v2-path-entry->amos url id path-k path-v)
        http-method (open-api-method->http-edn path-k)]
    (merge
     path-entry
     (make-http-method http-method))))

(defn open-api-v2-methods->amos
  {:malli/schema
   [:=> [:cat
         [:=> [:cat OpenAPIV2Path] amos/OperationId]
         [:string {:min 1}]
         OpenAPIV2PathMethods] amos/AMOSOperationsValue]}
  [id-gen url path-methods]
  (set
   (mapv
    (fn [[method-k method-entry]]
      (merge
       (open-api-v2-path->amos url (id-gen method-entry) method-k method-entry)
       (make-http-url url)))
    path-methods)))

(defn open-api-v2-paths->amos
  {:malli/schema
   [:=> [:cat
         [:=> [:cat OpenAPIV2Path] amos/OperationId]
         OpenAPIV2Path] amos/AMOSOperationsValue]}
  [id-gen paths]
  (set
   (mapcat
    (fn [[url methods]]
      (open-api-v2-methods->amos id-gen url methods))
    paths)))

(defn open-api-v2-definition->amos
  {:malli/schema [:=> [:cat [:string {:min 1}] SchemaObject] amos/AMOSDefinition]}
  [definition-name {:strs [type] :as definition}]
  (cond 
    (= type "object") {:data/schema (json-object->schema definition)
                       :data/name   definition-name
                       :data/key    (keyword "definitions" definition-name)}
    :else             {:data/schema [:any]}))

(defn open-api-v2-definitions->amos
  {:malli/schema [:=> [:cat OpenAPIV2JSONDefinitions] amos/AMOSDefinitionsValue]}
  [definitions]
  (into #{} (map (fn [[k v]] (open-api-v2-definition->amos k v)) definitions)))

(defn open-api-v2->amos
  {:malli/schema [:=>
                  [:cat
                   [:=> [:cat OpenAPIV2Specification] amos/AMOSId]
                   amos/AMOSNameValue
                   OpenAPIV2Specification]
                  amos/AMOS]}
  [id-gen amos-name open-api-v2-spec]
  (let [domain-meta
        (open-api-v2-meta-data->amos
         (select-keys
          open-api-v2-spec ["swagger" "info" "host" "basePath" "schemes" "consumes"]))
        operations
        (open-api-v2-paths->amos id-gen (get open-api-v2-spec "paths"))
        definitions
        (if-let [definitions (get open-api-v2-spec "definitions")]
          (open-api-v2-definitions->amos definitions)
          #{})]
    (merge
     domain-meta
     (amos/make-amos-definitions definitions)
     (amos/make-amos-operations operations)
     (amos/make-amos-name amos-name)
     (amos/make-amos-id (id-gen open-api-v2-spec)))))
