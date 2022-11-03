(ns user
  (:require
   [quickrest.alpha.main :as main]
   [integrant.core :as ig]
   [clojure.tools.logging :as log]
   [quickrest.alpha.repl :refer :all]))

(defn start []
  (println "Starting QuickREST Alpha...")
  (log/info "Starting development system")
  (alter-var-root #'main/profile (constantly :dev))
  (let [system-config (main/system-configuration)
        system        (ig/init system-config)]
    (alter-var-root #'main/system (constantly system)))
  (log/info "System started, enjoy!")
  ;;(status)
  :started)
