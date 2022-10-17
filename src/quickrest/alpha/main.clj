(ns quickrest.alpha.main
  (:require [clojure.tools.logging :as log]
            [quickrest.alpha.config :as config]
            [quickrest.alpha.clients.cli.main :as cli]
            [integrant.core :as ig])
  (:gen-class))

(def system nil)

(def profile :prod)

(def ^:const system-filename "system.edn")

(let [lock (Object.)]
  (defn- load-namespaces
    [system-config]
    (locking lock
      (ig/load-namespaces system-config))))

(defn configuration
  []
  (config/read-config system-filename {:profile profile}))

(defn system-configuration
  []
  (let [config        (configuration)
        system-config (:ig/system config)]
    (load-namespaces system-config)
    (ig/prep system-config)))

(defn stop-app
  []
  (some-> (deref system) (ig/halt!))
  (shutdown-agents))

(defn -main [& args]
  (println "Starting QuickRest...")
  (alter-var-root #'profile (constantly :dev))
  (let [system-config (system-configuration)
        system        (ig/init system-config)]
    (alter-var-root #'system (constantly system)))
  (log/info "System started, enjoy!")
  (cli/main system args))
