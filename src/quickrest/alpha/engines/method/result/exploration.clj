(ns quickrest.alpha.engines.method.result.exploration
  (:require [quickrest.alpha.resources.exploration.result :as rer]
            [clojure.tools.logging :as log]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Results

(defn with-exploration-result
  [check-property-fn property-k operation]
  (let [start-time   (. System (nanoTime))
        check-result (check-property-fn)
        time-taken   (/ (double (- (. System (nanoTime)) start-time)) 1000000.0)
        _
        (log/info (str "Start Exploration of " (:info/name (:operation/info operation))))
        result       (rer/make-result
                      (rer/make-session (rer/make-session-time-metric time-taken :msec))
                      (:shrunk-example check-result)
                      {:property/key property-k}
                      {:explored/operation operation})
        _            (log/info (str "Exploration done (" time-taken "ms)"))]
    result))
