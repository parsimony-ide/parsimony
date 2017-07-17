(ns parsimony.server.algo
  "Component that interfaces with underlying rdag algorithms"
  (:require [alanlcode.util.os :as os]
            [clojure.java.io :as io]
            [com.stuartsierra.component :as component]
            [parsimony.rdag.automaton :as automaton]
            [parsimony.rdag :as rdag]
            [parsimony.log :as log]))

(defrecord AlgoComponent [config]
  component/Lifecycle
  (start [this]
    (log/debug "AlgoComponent::start")
    (let [db-path (:db-path config)
          db-url (or (some-> (io/file db-path)
                             (os/nil-unless-exists)
                             (os/->url))
                     (io/resource db-path))
          db (rdag/restore-db db-url)]
      (log/info "Restored db description from" (str db-url))
      (assoc this :db db)))
  (stop [this]
    (log/debug "AlgoComponent::stop")
    this))

(defn new-algo [config]
  (map->AlgoComponent {:config (select-keys config [:db-path])}))

(defn horizon [this s]
  (rdag/horizon (:db this) s))

(defn all-packages
  "Return all packages. Does not include the BRICs automaton."
  [this]
  (let [packages (rdag/all-packages (:db this))
        f (fn [acc k v]
            (assoc acc k
                   (into {}
                         (map (fn [[k v]]
                                (vector
                                 k
                                 (select-keys v [:regex-str :source-str]))))
                         v)))]
    (reduce-kv f {} packages)))

(defn example-strings
  "Returns <= 10 example strings for the given regular expression in Parsimony syntax."
  [this path]
  (let [automaton (rdag/automaton-at (:db this) path)]
    (set (automaton/random-strings automaton 10 10))))
