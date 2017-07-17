(ns parsimony.rdag-test
  (:require [alanlcode.util.os :as os]
            [clojure.java.io :as io]
            [clojure.core.matrix :as matrix]
            [clojure.test :as t :refer [is deftest testing]]
            [fipp.edn :refer [pprint]]
            [loom.alg]
            [loom.graph]
            [parsimony.common-test :refer [temp-db-file]]
            [parsimony.rdag :as rdag]
            [parsimony.rdag.automaton :as automaton]
            [taoensso.timbre.profiling :refer [profile]]))

(defn- verbose-horizon [db s]
  (let [h (time (rdag/horizon db s))]
  (pprint
    [s (into {}
             (map (fn [p] [p (rdag/regex-str-at db p)])
                  h))])
  h))

(defn- show-examples [db paths]
  (pprint
    (for [p paths]
      {:path p
       :regex (rdag/regex-str-at db p)
       :examples (take 5 (automaton/random-strings (rdag/automaton-at db p) 5 200))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low Level Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(matrix/set-current-implementation :vectorz)

(defn all-zeroes [m]
  (every? #(= 0.0 %) (matrix/eseq m)))

(defn all-ones [m]
  (every? #(= 1.0 %) (matrix/eseq m)))

(deftest basic-matrix-ops
  (testing "row and column fill"
    (let [m (matrix/zero-matrix 3 3)]
      (is (all-zeroes m))
      (matrix/fill! (rdag/row-view m 0) 1)
      (is (all-ones (matrix/get-row m 0)))
      (matrix/fill! m 0)
      (is (all-zeroes m))
      (matrix/fill! (rdag/col-view m 1) 1)
      (is (all-ones (matrix/get-column m 1)))))
  (testing "matrix multiplication"
    (let [m (matrix/matrix [[1 0 1]
                            [0 0 1]
                            [1 1 1]])
          m2 (matrix/mmul m m)]
      (is (not (identical? m m2)))
      (is (= m2
             (matrix/matrix [[2 1 2]
                             [1 1 1]
                             [2 1 3]]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High Level Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn check-horizon [db suite]
  (let [h (rdag/horizon db suite)]
    (doseq [s suite path h]
      (is (rdag/match-one db [s] path)))
    (doseq [path h]
      (is (rdag/match-one db suite path)))
    h))

(deftest test-1
  (let [db (rdag/create-db-from-scratch "test-inputs/test-packages" (temp-db-file))]
    (is (= #{["test" :A]} (check-horizon db ["a"])))
    (is (= #{["test" :B]} (check-horizon db ["b"])))
    (is (= #{["test" :C]} (check-horizon db ["c"])))
    (is (= #{["test" :D]} (check-horizon db ["d"])))
    (is (= #{["test" :E]} (check-horizon db ["e"])))
    (is (= #{["test" :F]} (check-horizon db ["f"])))
    (is (= #{["test" :ABC]} (check-horizon db ["a" "b" "c"])))
    (is (= #{["test" :ABCDE]} (check-horizon db ["a" "b" "c" "d"])))
    (is (= #{["test" :AB]} (check-horizon db ["a" "b"])))
    (is (= #{["test" :EF]} (check-horizon db ["e" "f"])))
    (is (= #{rdag/TOP} (check-horizon db ["a" "f"])))
    (is (rdag/lt db ["test" :A] ["test" :AB]))
    (is (rdag/gt db ["test" :AB] ["test" :A]))
    (is (rdag/lt db ["test" :AB] ["test" :ABC]))
    (is (rdag/gte db ["test" :EF] ["test" :EF]))
    (is (rdag/comparable? db ["test" :C] ["test" :CDE]))
    (is (not (rdag/comparable? db ["test" :ABC] ["test" :DEF])))))

(deftest test-2
  (let [db (rdag/restore-db (io/resource "rdag.db"))]
    (is (= (rdag/automaton-at db ["fasta" :TEXT])
           (rdag/automaton-at db ["fasta" :TEXT])))
    (is (= (rdag/match-one db ["x"] ["fasta" :TEXT])
           (rdag/match-one db ["x"] ["fasta" :TEXT])))
    (doseq [t ["ab" "foo" "bar" "baz" "ident1" "hello-there" "123" "不好"]]
      (verbose-horizon db [t]))
    (doseq [t [["foo" "foo1"]
               ["1" "2" "3" "10" "999"]
               ["// hello" "// this is a comment"]
               ["/* hello there */" "// this is also a comment"]
               ["4'b1001" "1'b0" "1'sb1"]]]
      (println (apply str (repeat 80 "*")))
      (println t)
      (println (apply str (repeat 80 "*")))
      (show-examples db (check-horizon db t)))
    (show-examples db [["fasta" :TEXT]])))
