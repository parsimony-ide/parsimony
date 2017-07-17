(ns parsimony.rdag.automaton
  "Adaptor for dk.brics.automaton"
  (:import [dk.brics.automaton RegExp Automaton RunAutomaton State])
  (:require [clojure.pprint :refer [pprint]]
            [schema.core :as s]
            [ubergraph.alg :as uber.alg]
            [ubergraph.core :as uber]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BRICS Automata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def automaton-klass Automaton)
(def run-automaton-klass RunAutomaton)

(defn str->regexp [s]
  (RegExp. s RegExp/ALL))

(defn regexp->automaton [regexp]
  (doto (.toAutomaton regexp)
    (.minimize)))

(defn str->automaton [s]
  (-> s
      (str->regexp)
      (regexp->automaton)))

(defn automaton->dot [a]
  (spit "automaton.dot" (.toDot a)))

(defn optimize [a]
  (RunAutomaton. a))

(defn match? [a s]
  (.run a s))

(defn subset? [a1 a2]
  (.subsetOf a1 a2))

(defn finite? [a]
  (.isFinite a))

(defn finite-strings
  ([a]
   (.getFiniteStrings a))
  ([a n]
   (.getFiniteStrings a n)))

(defn strings-of-length [a n]
  ;; this is a dangerous function because it can blow out the heap since it eagerly computes ALL strings of length n
  ;; TODO: implement a limit on the number of strings generated
  (.getStrings a n))

(defn shortest-example [a]
  (.getShortestExample a true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BRICS->CLJS Conversion
;; - Rationale: BRICS is Java only, but we want to be able to run automata on
;;   the browser
;; - We don't want to reimplement all of BRICS in Javascript/CLJS, but
;;   we can convert a BRICS Automaton into a lightweight data structure to be
;;   consumed by corresponding CLJS code that runs in the browser.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; XXX: We're using super sketchy reflection code to get at protected members inside the RunAutomaton class.
;;      This is so we don't have to fork the BRICS code to change member accessibility.

(defn get-private-field [instance field-name]
  (when-let [field (doto (first (filter #(= field-name (.getName %))
                                        (.. instance getClass getDeclaredFields)))
                     (.setAccessible true))]
    (.get field instance)))

(defn invoke-private-method [instance fn-name & args]
  (when-let [method (doto (first (filter #(= fn-name (.getName %))
                                         (.. instance getClass getDeclaredMethods)))
                      (.setAccessible true))]
    (.invoke method instance (into-array Object args))))

(defn invoke-private-static-method [clazz fn-name & args]
  (when-let [method (doto (first (filter #(= fn-name (.getName %))
                                         (.. clazz getDeclaredMethods)))
                      (.setAccessible true))]
    (.invoke method nil (into-array Object args))))

(s/defschema js-automaton-transition-schema
  {:to s/Int
   :min Character
   :max Character})

(s/defschema js-automaton-schema
  {:initial s/Int
   :states
   {s/Int {:id s/Int
           :accept s/Bool
           :transitions [js-automaton-transition-schema]}}})

(defn transition->js-transition [t]
  {:to (get-private-field (get-private-field t "to")
                          "number")
   :min (get-private-field t "min")
   :max (get-private-field t "max")})

(defn automaton->js-automaton [a]
  {:post [(s/validate js-automaton-schema %)]}
  (let [a (doto a (.minimize))]
    (invoke-private-static-method Automaton "setStateNumbers" (.getStates a))
    (let [states (.getStates a)
          js-automaton
          (reduce
           (fn [js-automaton state]
             (let [state-number (get-private-field state "number")]
               (assoc-in js-automaton [:states state-number]
                         {:id state-number
                          :accept (.isAccept state)
                          :transitions (into []
                                             (map transition->js-transition)
                                             (.getTransitions state))})))
           {}
           states)]
      (assoc js-automaton :initial
             (get-private-field (.getInitialState a) "number")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automaton Graphs (Ubergraphs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn automaton->graph
  "Convert an automaton into a loom graph for easy query/manipulation"
  [a]
  (let [g (atom (uber/multidigraph))
        rename-node (into {}
                          (comp
                           (map hash)
                           (map-indexed #(vector %2 %1)))
                          (.getStates a))
        encode (fn [s] (rename-node (hash s)))]
    (swap! g uber/add-nodes* (map encode (.getStates a)))
    (doseq [state (.getStates a)
            transition (.getTransitions state)]
      (let [edge-min (.getMin transition)
            edge-max (.getMax transition)
            edge [(encode state) (encode (.getDest transition)) {:min edge-min :max edge-max}]]
        (swap! g uber/add-edges edge)))
    {:g @g
     :start (encode (.getInitialState a))
     :accept-states
     (into #{}
           (comp
            (filter #(.isAccept %))
            (map encode))
           (.getStates a))}))

(defn accept? [g node]
  (contains? (:accept-states g) node))

(defn succs [g node]
  (uber/successors (:g g) node))

(defn has-succ? [g node]
  (seq (succs g node)))

(defn shortest-length [g]
  (apply min
         (for [s (:accept-states g)]
           (dec (count (uber.alg/shortest-path (:g g) (:start g) s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example String Generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn random-succ [g node]
  (let [ss (vec (succs g node))]
    (nth ss (rand-int (count ss)))))

(defn -random-paths-up-to [g n max-tries length tries s so-far]
  (when (< tries max-tries)
    (let [restart
          (fn restart []
            (-random-paths-up-to g n max-tries n (inc tries) (:start g) []))
          random-backtrack
          (fn []
            (let [jump-amount (min (rand-int (- n length))
                                   (max (dec (count so-far))
                                        0))
                  so-far (subvec so-far 0 (- (count so-far) jump-amount))]
              #_(println "random-backtrack" jump-amount so-far)
              (-random-paths-up-to g n max-tries (+ length jump-amount) (inc tries) (last so-far) (subvec so-far 0 (dec (count so-far))))))]
      (cond
      ;;;;;;;;;;;;;;;;;;;;;;
      ;; accept state found
      ;;;;;;;;;;;;;;;;;;;;;;
        (accept? g s)
        (do #_(println "accept")
            (lazy-seq
             (cons (conj so-far s)
                   (if (and (not= length 0)
                            (has-succ? g s))
                     (-random-paths-up-to g n max-tries (dec length) (inc tries) (random-succ g s) (conj so-far s))
                     (random-backtrack)))))
      ;;;;;;;;;;;;;;;;;;;;;;
      ;; max-length reached without finding accept state
      ;;;;;;;;;;;;;;;;;;;;;;
        (= length 0)
        (do #_(println "zero")
            (restart))
      ;;;;;;;;;;;;;;;;;;;;;;
      ;; max-length not yet exceeded, and neighbor exists
      ;;;;;;;;;;;;;;;;;;;;;;
        (has-succ? g s)
        (do #_(println "continue")
            (-random-paths-up-to g n max-tries (dec length) tries (random-succ g s) (conj so-far s)))
      ;;;;;;;;;;;;;;;;;;;;;;
      ;; fallthrough
      ;;;;;;;;;;;;;;;;;;;;;;
        :else
        (do #_(println "fallthrough")
            (restart))))))

(defn random-paths-up-to [g n max-tries]
  (let [shortest (shortest-length g)
        n (max shortest n)]
    (-random-paths-up-to g n max-tries n 0 (:start g) [])))

(defn choose-1-random [xs]
  (nth (seq xs) (rand-int (count xs))))

(defn random-char-in-range [min-char max-char]
  (let [min-int (int min-char)
        max-int (int max-char)]
    (char (+ min-int (rand-int (inc (- max-int min-int)))))))

(defn -random-strings [g n max-tries]
  (letfn [(->random-char [endpoint]
            (let [edge (choose-1-random (apply uber/find-edges (:g g) endpoint))
                  {:keys [min max]} (uber/attrs (:g g) edge)]
              (random-char-in-range min max)))
          (->random-string [nodes]
            (let [endpoints (partition 2 1 nodes)]
              (apply str (map ->random-char endpoints))))]
    #_(map (juxt identity ->random-string) (random-paths-up-to g n))
    (map ->random-string (random-paths-up-to g n max-tries))))

(defn random-strings
  ([a n]
   (random-strings a n 200))
  ([a n max-tries]
   (-random-strings (automaton->graph a) n max-tries)))
