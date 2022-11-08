(ns unit.quickrest.alpha.engines.method.exploration-v2.shrinking-test
  (:require [quickrest.alpha.engines.method.exploration-v2.meta-operation-generators
             :as mog]
            [clojure.test :refer [deftest testing is]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [malli.generator :as mg]
            [clojure.test.check :as tc]
            [clojure.test.check.rose-tree :as rose]))

(comment
  (def shrink-gen
    (gen/gen-pure
     (mog/shrink-operation-sequence-2
      mog/shrinked-alternatives
      (fn [alternative-sequence] true)
      ;; (fn [alternative-sequence]
      ;;   (and (mog/valid-reference-operation-sequence? alternative-sequence)
      ;;        (= (:operation/id (first alternative-sequence))
      ;;           (:operation/id (last alternative-sequence))
      ;;           (:operation/id (:session/query-operation ctx)))))
      [1 2 3])))

  (def rose-tree
    (mog/shrink-operation-sequence-2
      mog/shrinked-alternatives
      (fn [alternative-sequence] true)
      ;; (fn [alternative-sequence]
      ;;   (and (mog/valid-reference-operation-sequence? alternative-sequence)
      ;;        (= (:operation/id (first alternative-sequence))
      ;;           (:operation/id (last alternative-sequence))
      ;;           (:operation/id (:session/query-operation ctx)))))
      [1 2 3]))
  
  (gen/sample shrink-gen)

  (rose/root rose-tree)
  (map rose/root (rose/children rose-tree))
  )

(defn rose-child-view
  [rose]
  [(rose/root rose) (mapv rose-child-view (rose/children rose))])

(defn rose-view
  [op-sequence]
  (let [rose (mog/shrink-operation-sequence-2
              mog/shrinked-alternatives
              ;;(fn [alternative-sequence] false)
              (fn [alternative-sequence]
                (println alternative-sequence)
                true
                ;;(and (mog/valid-reference-operation-sequence? alternative-sequence))
                ;; (= (:q-op (first alternative-sequence))
                ;;    (:q-op (last alternative-sequence)))
                )
              op-sequence)]
    [(rose/root rose) (mapv rose-child-view (rose/children rose))]))

(comment
  (count (flatten (rose-view [:q-op :del :q-op :post :q-op])))
  )

(defn valid-state-identity-observation-sequence?
  [alternative-sequence]
  )
