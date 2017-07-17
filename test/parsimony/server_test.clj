(ns parsimony.server-test
  (:require [clojure.edn :as edn]
            [clojure.test :as t :refer [deftest is testing use-fixtures]]
            [com.stuartsierra.component :as component]
            [fipp.edn :refer [pprint]]
            [parsimony.log :as log]
            [parsimony.rdag :as rdag]
            [parsimony.server :refer :all]
            [ring.mock.request :as mock]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Rig Setup and Teardown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce test-system nil)

(def test-config
  {:port 8999 :db-path "test-rdag.db"})

(defn init-test-system
  "Constructs the current development system."
  [config]
  (alter-var-root #'test-system
                  (constantly (system config))))

(defn start-test-system
  "Starts the current development environment."
  []
  (alter-var-root #'test-system component/start))

(defn stop-test-system
  "Shuts down and destroys the current development system."
  []
  (alter-var-root #'test-system
                  (fn [s] (when s (component/stop s)))))

(defn reset-test-system [config]
  (stop-test-system)
  (init-test-system config)
  (start-test-system))

(defn top-handler [system]
  (get-in system [:handler :handler]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn do-horizon-request [system strings]
  (let [req (mock/request :get "/horizon"
                          {:strings strings})
        handler (top-handler system)]
    (edn/read-string (:body (handler req)))))

(defmacro check [syms strings]
  `(is (= ~(set (for [s syms] ["test" s]))
          (set (do-horizon-request test-system ~strings)))))

(defn wrap-test
  [f]
  (init-test-system test-config)
  (start-test-system)
  (f)
  (stop-test-system))

(use-fixtures :once wrap-test)

(deftest t-horizon-1
  (check [:A] ["a"])
  (check [:B] ["b"])
  (check [:C] ["c"])
  (check [:D] ["d"])
  (check [:E] ["e"])
  (check [:F] ["f"])
  (check [:AB] ["a" "b"])
  (check [:ABC] ["a" "b" "c"])
  (check [:ABC] ["a" "c"])
  (check [:BCD] ["b" "c" "d"])
  (check [:ABC :BCD] ["b" "c"])
  (check [:CDE] ["c" "d" "e"])
  (check [:CDE] ["c" "e"])
  (check [:BCD :CDE] ["c" "d"])
  (check [:DEF] ["d" "e" "f"])
  (check [:CDE :DEF] ["d" "e"])
  (check [:CDEF] ["c" "d" "e" "f"])
  (check [:ABCDE] ["a" "b" "c" "d" "e"])
  (check [:ABCDE] ["a" "d"])
  (check [:ABCDE] ["a" "e"])
  (check [:EF] ["e" "f"])
  (is (= #{rdag/TOP} (set (do-horizon-request test-system ["a" "f"])))))
