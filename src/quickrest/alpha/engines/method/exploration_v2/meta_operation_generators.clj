(ns quickrest.alpha.engines.method.exploration-v2.meta-operation-generators
  (:require [clojure.test.check.rose-tree :as rose]
            [clojure.test.check.generators :as gen]
            [malli.generator :as mg]
            [quickrest.alpha.resources.schema :as rsc]
            [clojure.tools.logging :as log]))

;;;;
;; Basic generators

;;;;
;; Given an operation with a schema, can randomly generate

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


;; To make things flow, and to be more observable, we should attach generated values(?)

(defn random-parameter
  [schema-generator {:keys [data/schema] :as parameter}]
  (gen/fmap
   (fn [param-value]
     (let [gen-value {:generation/value param-value
                      :generation/id    (random-uuid)}]
       (-> parameter
           (assoc :parameter/generation gen-value)
           (assoc :parameter/value (:generation/value gen-value)))))
   (schema-generator schema)))

(comment
  (gen/sample
   (random-parameter mg/generator {:data/schema    [:map ["productName" :string]],
                                   :parameter/name "productName",
                                   :http/in        :path})
   10)
  )

(defn make-parameter-reference
  [reference-id {:keys [:parameter/value :parameter/generation] :as reference-parameter}]
  {:generation/value     value
   :generation/reference {:reference/operation-id  reference-id
                          :reference/generation-id (:generation/id generation)}
   :generation/id        (random-uuid)})

(defn with-parameter-reference
  [m reference]
  (-> m
      (assoc :parameter/generation reference)
      (assoc :parameter/value (:generation/value reference))))

(defn reference-parameter
  "From all valid references, select one value"
  [reference-generator {:keys [data/schema] :as parameter}]
  (gen/fmap
   (fn [{:keys [reference-value reference-id]}]
     (let [gen-value {;;:generation/value     reference-value
                      :generation/reference reference-id
                      :generation/id        (random-uuid)}]
       (-> parameter
           (assoc :parameter/generation gen-value))))
   (reference-generator parameter)))

(defn find-reference
  [parameter]
  [{:reference-id (random-uuid) :reference-value "foo"}])

(comment
  (gen/sample
   (reference-parameter
    (fn [p] (gen/elements (find-reference p)))
    {:data/schema    [:map ["productName" :string]],
     :parameter/name "productName",
     :http/in        :path})
   10)
  )

(defn operation
  "Selects an operation from the given operations and generates values for all
  parameters"
  [ops]
  (gen/fmap
   (fn [[op gened-params]]
     (assoc op :operation/parameters gened-params))
   (gen/bind (gen/elements ops)
             (fn [operation]
               (gen/tuple
                (gen/return operation)
                (apply gen/tuple
                       (map #(random-parameter mg/generator %)
                            (:operation/parameters operation))))))))

(comment

  (gen/sample (operation example-operations))
  )

(defn operation-with-random-parameters
  "Given an operation, randomly generates for all the operations parameters"
  [op]
  (gen/fmap
   (fn [parameters-with-random-values]
     (log/debug op)
     (assoc op :operation/parameters parameters-with-random-values))
   (apply gen/tuple
          (map #(random-parameter mg/generator %)
               (:operation/parameters op)))))

(comment
  (gen/sample (operation-with-random-parameters example-operation))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequence shrinking

;; TODO - revisit!

(defn shrinked-alternatives
  [s]
  (map-indexed (fn [i _] (#'rose/exclude-nth i s)) s))

(defn valid-reference-operation-sequence?
  [operation-sequence]
  (vector?
   (reduce
    (fn [acc parameter]
      (let [generation (:parameter/generation parameter)
            param-ref  (:generation/reference generation)
            op-ref     (:reference/operation-id param-ref)
            gen-ref    (:reference/generation-id param-ref)]
        (if gen-ref
          (if (seq
               (filter
                (fn [prev-param]
                  (let [param-gen-id (get-in prev-param [:parameter/generation
                                                         :generation/id])]
                    (= param-gen-id gen-ref)))
                ;; acc contains the processed parameters
                acc))
            (conj acc parameter)
            (do
              ;;(println (str "Did not find " gen-ref))
              (reduced false)))
          (conj acc parameter))))
    []
    (mapcat :operation/parameters operation-sequence))))

;; (defn valid-amos-operation-sequence?
;;   [amos-operation-sequence]
;;   (vector?
;;    (reduce
;;     (fn [acc parameter]
;;       (let [param     (:operation/parameter amos-operation)
;;             param-ref (:operation.parameter/reference param)]
;;                ;;(println (str "Param => " param))
;;         (if param-ref
;;           (if (seq
;;                (filter
;;                 (fn [operation]
;;                   (let [op-id (:operation.key/id
;;                                (:operation/key operation))
;;                         p-id  (:operation.parameter/id
;;                                (:operation/parameter operation))]
;;                     ;; (println (str "Comparing => " operation))
;;                     ;; (println (str "Op id - " op-id " p-id " p-id))
;;                     (or (= op-id param-ref)
;;                         (= p-id param-ref))))
;;                 acc))
;;             (conj acc amos-operation)
;;             (do
;;               ;;(println (str "Did not find " (:ref param)))
;;               (reduced false)))
;;           (conj acc amos-operation))))
;;     []
;;     (mapcat :operation/parameters amos-operation-sequence))))

(defn shrink-operation-sequence
  [amos-operation-sequence]
  ;;(println (str "OPSEQ " amos-operation-sequence))
  (when (seq amos-operation-sequence)
    (rose/make-rose amos-operation-sequence
                    (->> (shrinked-alternatives amos-operation-sequence)
                         (filter (fn [operation-sequence]
                                   (valid-reference-operation-sequence?
                                    operation-sequence)))
                         (map (fn [operation-sequence]
                                (shrink-operation-sequence operation-sequence)))
                         (filter some?)))))

(defn shrink-operation-sequence-2
  "A recursive shrinking that will produce the shrinking alternatives,
  from `shrink-sequence-fn` and filter them through the `valid-sequences-fn`"
  [shrink-sequence-fn valid-sequence? operation-sequence]
  ;; We are done with the recursive part of the rose tree if the seq is empty
  (when (seq operation-sequence)
    (rose/make-rose
     ;; start with the current sequence
     operation-sequence
     ;; produce the new seq of alternatives
     (->> (shrink-sequence-fn operation-sequence)
          (filter (fn [alternative-sequence]
                    (valid-sequence? alternative-sequence)))
          (map (fn [valid-sequence]
                 (shrink-operation-sequence-2
                  shrink-sequence-fn valid-sequence? valid-sequence)))
          (filter some?)))))

;;;;
;; Generate a recursive sequence of operations. For each iteration look if there is any previous operation value that can be referenced.

;; should there be different sequence generators? I.e. should there be one that always tries to look for references?
;; - yeah, it should be possible to inject the strategy for operation selection

;; Parameter independance, when an operation have been selected, the generation should be on parameter level, not operation level

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Meta-operation sequence

(defn meta-operation
  "Generate one meta-operation with the given parameter generator. The operation will
  be selected from the given operations."
  [{:keys [operations operation-sequence parameter-generator]}]
  (gen/fmap
   (fn [[op generated-params]]
     ;;(log/info (str "meta-operation => " op " - " generated-params))
     (assoc op :operation/parameters generated-params))
   (gen/bind
    ;; Select an operation
    (gen/elements operations)
    (fn [selected-operation]
      (gen/tuple
       (gen/return selected-operation)
       ;; Foreach param, try reference, revert to random (this should be defered to the param generator)
       (apply gen/tuple
              (map (fn [parameter]
                     (parameter-generator
                      selected-operation operation-sequence parameter))
                   (:operation/parameters selected-operation))))))))

(defn meta-operation-sequence
  [{:keys [operations operation-generator-frequencies parameter-generator] :as ctx}
   operation-sequence
   nrof-operations]
  (gen/bind
   (meta-operation (assoc ctx :operation-sequence operation-sequence))
   (fn [selected-meta-operation]
     (let [new-operation-sequence (conj operation-sequence selected-meta-operation)]
       ;; check if we are done with the sequence
       (if (zero? nrof-operations)
         ;; if done, return the generated operation of this iteration
         (gen/return new-operation-sequence)
         ;; otherwise, conj the operation, add it to the sequence, and recur
         (meta-operation-sequence ctx new-operation-sequence (dec nrof-operations)))))))

(comment
  (gen/sample (meta-operation-sequence
               {:operation-generator-frequencies [[1 gen/string] [7 gen/int]]
                :operations                      example-operations
                :parameter-generator
                (fn [operation operation-sequence parameter]
                  (random-parameter mg/generator parameter))}
               []
               2)
              1)
  )


(defn identity-meta-operation-sequence
  [ctx operation nrof-instances]
  (gen/fmap
   (fn [operation-with-parameters]
     (vec (repeat nrof-instances operation-with-parameters)))
   (operation-with-random-parameters operation)))

(comment
  (gen/sample (identity-meta-operation-sequence {} example-operation 3))
  )

(defn filter-parameters
  [valid-refs operation]
  (let [filtered-params (filterv
                         (fn [parameter]
                           (let [param-nodes (:schema/nodes parameter)
                                 node-ids    (keys param-nodes)]
                             (some (into #{} node-ids) valid-refs)))
                         (:operation/parameters operation))]
    (assoc operation :operation/parameters filtered-params)))

(comment
  (filter-parameters
   ["map.entry/productName::string"]
   (rsc/transform-operation {:gen-name-f (fn [_] (random-uuid))} example-operation)
   )
  )

(defn reference-lookup
  [valid-refs operation-sequence operation parameter]
  ;; from the sequence of operations, filter the valid params to select from
  (let [;;operation-seq-with-schema-nodes
        ;; TODO - this should be pre-calculated
        ;; (mapv #(rsc/transform-operation {:gen-name-f (fn [_] (random-uuid))} %)
        ;;       operation-sequence)
        operations-with-valid-parameters
        (filterv
         (fn [op]
           (seq (:operation/parameters op)))
         (mapv #(filter-parameters valid-refs %) operation-sequence
               ;;operation-seq-with-schema-nodes
               ))]
    operations-with-valid-parameters)
  )

(comment
  (reference-lookup
   ;; what are the valid refs?
   ["map.entry/productName::string"
    "map.entry/sourceFeature::string"
    "vector->string"]
   ;; the sequence to select from
   [{:operation/id        #uuid "5eb796a4-0fa4-4655-8370-ba9a0ba58dfe",
     :operation/info
     #:info{:name "addExcludesConstraintToProduct",
            :key  :addExcludesConstraintToProduct},
     :operation/parameters
     [{:data/schema     [:map ["productName" :string]],
       :parameter/name  "productName",
       :parameter/value "foo"
       :http/in         :path}
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
     :http/url            "/products/{productName}/constraints/excludes"}
    {:operation/id        #uuid "5eb796a4-0fa4-4655-8370-ba9a0ba58df1",
     :operation/info
     #:info{:name "addExcludesConstraintToFoo",
            :key  :addExcludesConstraintToFoo},
     :operation/parameters
     [{:data/schema     [:map ["productFoo" :int]],
       :parameter/name  "productFoo",
       :parameter/value "foo"
       :http/in         :path}],
     :operation/responses [#:data{:schema [:map-of :int :any]}],
     :http/method         :post,
     :http/url            "/products/{productName}/constraints/excludes"}]
   ;; the current operation context
   {:operation/id        #uuid "5eb796a4-0fa4-4655-8370-ba9a0ba58dfe",
    :operation/info
    #:info{:name "addExcludesConstraintToProduct",
           :key  :addExcludesConstraintToProduct},
    :operation/parameters
    [{:data/schema     [:map ["productName" :string]],
      :parameter/name  "productName",
      :http/in         :path}
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
    :http/url            "/products/{productName}/constraints/excludes"}
   ;; the current parameter
   {:data/schema    [:map ["productName" :string]],
    :parameter/name "productName",
    :http/in        :path}
   )

  (rsc/transform-operation
   {:gen-name-f (fn [_] (random-uuid))}
   example-operation)
  )

;; look backwards in the sequence for parameters that match the schema
;; - Do we need the graph to provide us with a param->op match?
(defn reference-param
  [valid-refs operation-sequence schema-generator {:keys [data/schema parameter/name] :as parameter}]
  (let [operations-to-select-from
        (reference-lookup valid-refs operation-sequence :empty :empty)]
    (if (seq operations-to-select-from)
      ;; there are valid references
      (gen/fmap
       (fn [[op v]]
         (with-parameter-reference
           parameter
           (make-parameter-reference (:operation/id op) v)))
       (gen/bind
        (gen/elements operations-to-select-from)
        (fn [selected-op]
          (let [params (:operation/parameters selected-op)
                ;;values (filter identity (map :parameter/value params))
                ]
            (gen/tuple (gen/return selected-op) (gen/elements params))))))
      ;; revert to random if none is found
      (random-parameter schema-generator parameter)))
  )

(comment

  (gen/sample (reference-param
               ["map.entry/productName::string"
                "map.entry/sourceFeature::string"
                "vector->string"]
               [{:operation/id        #uuid "5eb796a4-0fa4-4655-8370-ba9a0ba58dfe",
                 :operation/info
                 #:info{:name "addExcludesConstraintToProduct",
                        :key  :addExcludesConstraintToProduct},
                 :operation/parameters
                 [{:data/schema     [:map ["productName" :string]],
                   :parameter/name  "productName",
                   :parameter/value "foo"
                   :http/in         :path}
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
                 :http/url            "/products/{productName}/constraints/excludes"}]
               mg/generator
               {:data/schema    [:map ["productName" :string]],
                :parameter/name "productName",
                :http/in        :path})
              1)

  (def example-params
    [{:data/schema    [:map ["productName" :string]],
      :parameter/name "productName",
      :http/in        :path}
     {:data/schema    [:map ["sourceFeature" {:optional true} :string]],
      :parameter/name "sourceFeature",
      :http/in        :form-data}
     {:data/schema    [:map ["excludedFeature" {:optional true} :string]],
      :parameter/name "excludedFeature",
      :http/in        :form-data}])
  
  ;; reference parameter gen
  (gen/sample (sized-meta-operation-sequence
               {:operations example-operations
                :parameter-generator
                (fn [operation-sequence parameter]
                  (reference-param operation-sequence mg/generator parameter))}
               [:foo :bar :baz]
               2
               shrink-operation-sequence)))

(defn calculate-max-size
  [min-size max-size suggested-size]
  (let [not-larger-than-max (min suggested-size max-size)]
    (if (> not-larger-than-max min-size)
      not-larger-than-max
      min-size)))

(defn sized-meta-operation-sequence
  [ctx pre-sequence min-size max-size shrink-operation-sequence]
  (gen/sized
   (fn [size]
     (gen/bind
      (gen/choose min-size (calculate-max-size min-size max-size size))
      (fn [nrof-operations]
        (gen/bind
         (meta-operation-sequence ctx pre-sequence nrof-operations)
         (fn [generated-operation-sequence]
           (gen/gen-pure (shrink-operation-sequence
                          ctx generated-operation-sequence)))))))))

(comment
  (gen/sample (sized-meta-operation-sequence
               ;; operations should also be an injected generator
               {:operations example-operations
                :parameter-generator
                (fn [operation operation-sequence parameter]
                  ;;(random-parameter mg/generator parameter)
                  (reference-param
                   ;; look these up for the given parameter
                   ["map.entry/productName::string"
                    "map.entry/sourceFeature::string"
                    "vector->string"]
                   operation-sequence
                   mg/generator
                   parameter)
                  )}
               []
               2
               shrink-operation-sequence)))

(defn meta-operation-sequence-such-that
  [ctx pred pre-sequence min-size max-size shrink-operation-sequence]
  (gen/such-that
   (fn [operations]
     (pred operations))
   (sized-meta-operation-sequence
    ctx pre-sequence min-size max-size shrink-operation-sequence)))

;; (defn identity-meta-operation-sequence
;;   [ctx operation nrof-instances]
;;   (gen/fmap
;;    (fn [operation-with-parameters]
;;      (vec (repeat nrof-instances operation-with-parameters)))
;;    (operation-with-random-parameters operation)))

(defn state-mutation-sequence
  [ctx query-operation operations reference-lookup-fn shrink-sequence-fn min-size max-size]
  (gen/fmap
   (fn [[q-op op-seq]]
     (conj op-seq q-op))
   (gen/bind
    (operation-with-random-parameters query-operation)
    (fn [gened-query-operation]
      (gen/tuple (gen/return gened-query-operation)
                 (;;sized-meta-operation-sequence
                  meta-operation-sequence-such-that
                  ;; operations should also be an injected generator
                  {:operations operations
                   :parameter-generator
                   (fn [operation operation-sequence parameter]
                     ;;(random-parameter mg/generator parameter)
                     (gen/frequency
                      [[1 (reference-param
                           ;; look these up for the given parameter
                           (reference-lookup-fn operation operation-sequence parameter)
                           operation-sequence
                           mg/generator
                           parameter)]
                       [1 (random-parameter mg/generator parameter)]]))}
                  (fn [operations]
                    (if (< (count operations) 2)
                      true
                      (let [ids (into #{} (mapv :operation/id operations))]
                        (= (count ids) (count operations))))) ;; pred
                  [gened-query-operation] ;; pre-seq
                  (or min-size 0)
                  (or max-size 5)
                  (fn [ctx operation-sequence]
                    (-> ctx
                        (assoc :session/query-operation gened-query-operation)
                        (shrink-sequence-fn operation-sequence)))))))))

(defn state-identity-with-observation-sequence
  "Produces a sequence of [q-op, rand-op, q-op, ..., q-op], a random sequence of
  operations, enclosed by query ops. In addition, the query op is interlaced in the seq.
  The reason is to be able to check for observations within the sequence."
  [ctx query-operation operations reference-lookup-fn shrink-sequence-fn min-size max-size]
  (gen/fmap
   (fn [[q-op op-seq]]
     ;; Note: the first query op is in the pre-seq, thus already added
     (conj (into [q-op] (interpose q-op (rest op-seq))) q-op))
   (gen/bind
    ;; generate parameters for the given query operation
    (operation-with-random-parameters query-operation)
    ;; generate the sequence of operations
    ;; NOTE: we do not want to include the interlaced q-ops here, they are not
    ;;       valid reference targets, they should be added when finalizing the seq.
    (fn [gened-query-operation]
      (gen/tuple (gen/return gened-query-operation)
                 (;;sized-meta-operation-sequence
                  meta-operation-sequence-such-that
                  ;; TODO: operations should also be an injected generator
                  {:operations operations
                   :parameter-generator
                   (fn [operation operation-sequence parameter]
                     ;; TODO: think about how to handle this mix
                     (gen/frequency
                      [[1 (reference-param
                           ;; look these up for the given parameter
                           (reference-lookup-fn operation operation-sequence parameter)
                           operation-sequence
                           mg/generator
                           parameter)]
                       [1 (random-parameter mg/generator parameter)]]))}
                  (fn [operations]
                    (if (< (count operations) 2)
                      true
                      (let [ids (into #{} (mapv :operation/id operations))]
                        (= (count ids) (count operations)))))
                  ;; The given pre-seq; in this case, we start with the query op
                  [gened-query-operation]
                  (or min-size 0)
                  (or max-size 5)
                  (fn [ctx operation-sequence]
                    (-> ctx
                        (assoc :session/query-operation gened-query-operation)
                        (shrink-sequence-fn operation-sequence)))))))))

(comment
  (conj (interpose :q-op (rest [:q-op :b :c :d])) :q-op)

  (conj (into [:q-op] (interpose :q-op (rest [:q-op :b :c :d]))) :q-op)
  )
