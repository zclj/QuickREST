(ns quickrest.alpha.utilities.schema
  (:require [malli.core :as m]
            [malli.error :as me]))

(defn make-registry
  "Creates the type registry needed for schema operations.
  `defintions` - map of type-names to schemas"
  [definitions]
  {:registry (merge (m/default-schemas) definitions)})

(defn humanize
  [schema value registry]
  (let [opt {:registry (merge (m/default-schemas) registry)}]
    (-> schema
        (m/explain value opt)
        (me/humanize {:wrap #(select-keys % [:value :message])}))))

(defn validate
  [schema value options]
  (m/validate schema value (update options
                                   :registry
                                   (fn [r] (merge (m/default-schemas) r)))))

