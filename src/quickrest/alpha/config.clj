(ns quickrest.alpha.config
  (:require [aero.core :as aero]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [integrant.core :as ig]))

(defmethod aero/reader 'ig/ref
  [_ _ value]
  (ig/ref value))

(defn read-config
  [filename options]
  (log/info "Reading config" filename)
  (log/infof "Configuration profile : %s" (name (:profile options)))
  (aero/read-config (io/resource filename) options))

