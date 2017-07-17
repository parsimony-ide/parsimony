(ns parsimony.example-test
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
  (reset! example-db (rdag/create-db-from-scratch "test-inputs/example-packages" (temp-db-file)))
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
                ["0" "1"]
                ["0" "9"]
                ["a"]
                ["a" "d"]
                ["a" "10"]
                ["a" "01"]]
        results (for [ss groups]
                  (let [horizons (map #(rdag/horizon @example-db (vector %)) ss)
                        merged (rdag/horizon @example-db ss)]
                    [ss {:horizons horizons
                         :merged merged}]))]
    (pprint results)))

(deftest t-example-3 []
  (let [strings ["X"
                 "XX"
                 "XXX"
                 "XXXX"
                 "MCMXIV"  ;; 1914
                 "MMXVII"] ;; 2017
        results (for [s strings]
                  [s (rdag/horizon @example-db [s])])]
    (pprint results)))


(deftest t-example-4 []
  (let [strings ["555-5555"
                 "555-555-5555"
                 "(555)-555-5555"
                 "+1-(555)-555-5555"
                 "1.(555).555.5555"
                 "15555555555"
                 "1.(555)-555-5555 ext. 5555"]
        results (for [s strings]
                  [s (rdag/horizon @example-db [s])])]
    (pprint results)))

(deftest t-example-5 []
  (let [groups [["1"]
                ["1" "5"]
                ["1" "5" "8"]
                ["1" "5" "8" "a"]
                ["1" "5" "8" "a" "aa"]]
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
  (t-example-3)
  (t-example-4)
  (t-example-5)
  (rdag/dot-graph @example-db))
