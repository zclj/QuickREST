(ns quickrest.alpha.clients.cli.main
  (:require [clojure.tools.cli :as cli]
            [clojure.string :as string]
            [quickrest.alpha.managers.specification.manager :as sm]
            [quickrest.alpha.managers.objective.manager :as om]
            [quickrest.alpha.resources.xtdb :as rxt])
  (:import (java.net InetAddress)))

(def cli-options
  [;; First three strings describe a short-option, long-option with optional
   ;; example argument description, and a description. All three are optional
   ;; and positional.
   ["-p" "--port PORT" "Port number"
    :default 80
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   ["-H" "--hostname HOST" "Remote host"
    :default (InetAddress/getByName "localhost")
    :default-desc "localhost"
    :parse-fn #(InetAddress/getByName %)]
   ["-f" "--file NAME" "File name of an OpenAPI Specification"
    :default []]
   ["-u" "--url NAME" "File name of an OpenAPI Specification"
    :default []]
   ["-b" "--behavior NAME" "The name of the behavior to explore"
    :default []
    :multi true
    :update-fn conj]
   ["-l" "--min-seq-size INT" "The maximum number of tests per behavior property"
    :default 1
    :parse-fn #(Integer/parseInt %)]
   ["-s" "--max-seq-size INT" "The maximum number of tests per behavior property"
    :default 1
    :parse-fn #(Integer/parseInt %)]
   ["-t" "--tests INT" "The maximum number of tests per behavior property"
    :default 100
    :parse-fn #(Integer/parseInt %)]

   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["Welcome to QuickREST"
        ""
        "Usage: program-name [options] action"
        ""
        "Options:"
        options-summary
        ""
        "Actions:"
        "  start    Start a new test generation"
        ""
        "Please refer to the manual page for more information."]
       (string/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with an error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [args]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)]
    (cond
      (:help options) ; help => exit OK with usage summary
      {:exit-message (usage summary) :ok? true}
      errors ; errors => exit with description of errors
      {:exit-message (error-msg errors)}
      ;; custom validation on arguments
      (and (= 1 (count arguments))
           (#{"start" "test"} (first arguments)))
      {:action (first arguments) :options options}
      :else ; failed custom validation => exit with usage summary
      {:exit-message (usage summary)})))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(comment
  (user/start)
  )

(defn start-test-execution
  [system {:keys [action options]}]
  ;; given a exploration-output file, run the examples
  (let [execution-result
        (om/execute-examples (:file options)
                             {:http/scheme  :http
                              :request/host (.getHostName (:hostname options))
                              :request/port (:port options)})
        failures? (not= 0 (:tests/failed (:summary execution-result)))
        exit-code (if failures? -1 0)]
    (clojure.pprint/pprint execution-result)
    (println (str "Failed " (:failed (:summary execution-result))))
    (exit exit-code (str "Test execution done! (exit-code " exit-code ")"))))

(defn start-exploration
  [system {:keys [action options]}]
  ;; 1. transform the OAS
  (let [service-amos       (sm/transform-open-api-v2-spec-to-amos
                            (:quickrest.alpha.resources.xtdb/xt-node system)
                            (or (:url options) (:file options))
                            (if (:url options) :source/url :source/file)
                            "No name")
        ;; 2. explore a property
        exploration-result (om/explore-properties
                            (:quickrest.alpha.resources.xtdb/xt-node system)
                            (:transformation/amos-id service-amos)
                            {:http/scheme  :http
                             :request/host (.getHostName (:hostname options))
                             :request/port (:port options)
                             :nrof-tests   (:tests options)
                             :max-seq-size (:max-seq-size options)
                             :min-seq-size (:min-seq-size options)}
                            (map keyword (:behavior options)))]
    (clojure.pprint/pprint exploration-result)
    (exit 0 "Exploration Done!")))

(defn main
  [system args]
  (let [{:keys [action options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (case action
        "start" (do (println (str "Starting with " options))
                     (start-exploration system (validate-args args)))
        "test"  (do (println (str "Test execution with " options))
                    (start-test-execution system (validate-args args)))
        ;;"stop"   (server/stop! options)
        ;;"status" (server/status! options)
        ))))

