(ns user
  (:require [alembic.still]
            [clojure.tools.namespace.repl]
            [com.stuartsierra.component :as component]
            [parsimony.server :as server]
            [potemkin :refer [import-vars]]))

(import-vars [alembic.still load-project]
             [clojure.tools.namespace.repl refresh])

(def system nil)

(defn init
  "Constructs the current development system."
  []
  (alter-var-root #'system
                  (constantly (server/system))))

(defn start
  "Starts the current development environment."
  []
  (alter-var-root #'system component/start))

(defn stop
  "Shuts down and destroys the current development system."
  []
  (alter-var-root #'system
                  (fn [s] (when s (component/stop s)))))

(defn go
  "Initializes the current development system and starts it running."
  []
  (init)
  (start))

(defn reset []
  (stop)
  (refresh :after 'user/go))
