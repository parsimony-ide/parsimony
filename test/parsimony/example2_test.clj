(ns parsimony.example2-test
  (:require [parsimony.common-test :refer [temp-db-file]]
            [parsimony.rdag :as rdag]
            [parsimony.rdag.automaton :as automaton]
            [fipp.edn :refer [pprint]]
            [loom.graph]
            [loom.io]
            [ubergraph.core :as uber]
            [clojure.core.matrix :as matrix]
            [clojure.test :as t :refer [deftest is testing use-fixtures]]
            [taoensso.nippy :as nippy]
            [taoensso.timbre.profiling :refer [profile]]))

(defonce example-db (atom nil))

(defn create-example-db! []
  (reset! example-db (rdag/create-db-from-scratch "test-inputs/example2-packages" (temp-db-file)))
  nil)

(defn wrap-test
  [f]
  (create-example-db!)
  (f)
  (reset! example-db nil))

(use-fixtures :once wrap-test)

(deftest t-example-1 []
  (let [strings ["0" "5" "8" "a" "e" "01" "10" "1234deadbeef" "0000"
                 "0.1" "0." "1e12" "6.022e+23"]
        results (for [s strings]
                  [s (rdag/horizon @example-db [s])])]
    (pprint results)))

(deftest t-example-2 []
  (let [groups [["0"]
                ["00"]
                ["5"]
                ["8"]
                ["a"]
                ["x"]
                ["1e12"]
                ["-1.2"]
                ["0" "5"]
                ["0" "5" "8"]
                ["0" "5" "8" "a"]
                ["0" "5" "-1.2"]
                ["1e12" "a"]
                ["1e12" "-1.2"]]
        results (for [ss groups]
                  (let [horizons (map #(rdag/horizon @example-db (vector %)) ss)
                        merged (rdag/horizon @example-db ss)]
                    [ss {:horizons horizons
                         :merged merged}]))]
    (pprint results)))

(comment
  (create-example-db!)
  (pprint @example-db)
  (pprint (rdag/horizon @example-db ["a"]))
  (t-example-1)
  (t-example-2)
  (rdag/dot-graph @example-db))
