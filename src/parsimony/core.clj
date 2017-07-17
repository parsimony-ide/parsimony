(ns parsimony.core
  (:require [alanlcode.util.os :as os]
            [alanlcode.util.package :as package]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]]
            [com.stuartsierra.component :as component]
            [parsimony.log :as log]
            [parsimony.rdag :as rdag]
            [parsimony.server :as server]
            [taoensso.nippy :as nippy])
  (:gen-class))

(def cli-options
  [["-h" "--help"             "Show help doc"]
   ["-V" "--version"          "Show version"]
   ["-c" "--create-db"        "Create RDAG db"]
   [nil  "--packages DIR"     "Create RDAG db from packages in given directory" :default "packages"]
   ["-o" "--output-file FILE" "Write RDAG db to given file"                     :default "rdag.db"]
   ["-s" "--serve"            "Start the HTTP server"]
   ["-p" "--port PORT"        "Port number"
    :default 9254
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]])

(defn usage [summary]
  (->> [(str "Parsimony Version " (package/version))
        ""
        "Usage: java -jar parsimony.jar [options]"
        ""
        "Options:"
        summary]
       (str/join \newline)))

(defn error-msg [errors]
  (str/join \newline
            (into ["The following fatal errors occurred:" ""]
                  errors)))

(defn create-db [package-dir output-file]
  (rdag/create-db-from-scratch package-dir output-file)
  nil)

(defn serve
  ([] (serve nil nil))
  ([port db-path]
   (let [{{http-server :server} :http}
         (component/start
           (server/system
             (cond-> {}
               port (into {:port port})
               db-path (into {:db-path db-path}))))]
     (log/info "Server started at" (str "http://" (:host http-server) ":" (:port http-server))))))

(defn -main
  [& args]
  (let [{:keys [options arguments summary errors]} (parse-opts args cli-options)]
    (try
      (cond
         (seq errors)         (println (error-msg errors))
         (empty? args)        (serve)
         (:help options)      (println (usage summary))
         (:version options)   (println (package/version))
         (:create-db options) (create-db (:packages options) (:output-file options))
         (:serve options)     (serve (:port options) (first arguments)))
      (catch Exception e
        (log/error e))))
  (shutdown-agents))
