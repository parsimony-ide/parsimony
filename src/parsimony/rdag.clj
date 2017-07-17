(ns parsimony.rdag
  (:import [java.io PushbackReader PrintWriter])
  (:require [alanlcode.util.os :as os]
            [clojure.core.matrix :as matrix]
            [clojure.core.matrix.stats :as matrix.stats]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [fipp.edn :as fipp]
            [jordanlewis.data.union-find :as uf]
            [loom.graph]
            [loom.io]
            [loom.alg]
            [parsimony.rdag.automaton :as automaton]
            [parsimony.common :refer [serialize deserialize]]
            [parsimony.log :as log]
            [parsimony.rdag.compiler :as compiler]
            [progrock.core :as pr]
            [schema.core :as s]))

(matrix/set-current-implementation :vectorz)

(s/defschema path-schema
  [(s/one s/Str "package-name") (s/one s/Keyword "token-name")])

(s/defschema package-schema
  {s/Str compiler/map-schema})

(s/defschema full-db-schema
  {:packages package-schema
   :m s/Any ;; matrix
   :encode {path-schema s/Int}
   :decode {s/Int path-schema}
   :equivalences [#{s/Int}]
   :canonical #{s/Int}
   :g s/Any ;; loom graph
   })

(s/defschema compact-db-schema
  (select-keys full-db-schema [:packages :g]))

(def BUILTIN-LANG "parsimony-builtin")
(def BUILTIN-DEF "TOP = .* ;")
(def TOP [BUILTIN-LANG :TOP])

(defn- init-db
  [package-dir]
  (let [file-paths
        (into []
              (comp
                (mapcat os/child-files)
                (filter #(= "tokens" (second (os/name-and-ext %)))))
              (os/child-directories package-dir))]
    (log/info "Compiling" (count file-paths) "packages:"
              (into [] (map (comp first os/name-and-ext)) file-paths))
    {:packages
     (-> {}
         ;; compile all packages in package-dir
         (into (map #(vector (first (os/name-and-ext %))
                             (compiler/make-file->map %)))
               file-paths)
         ;; add builtin definitions
         (assoc BUILTIN-LANG (compiler/make-str->map BUILTIN-DEF)))}))

(defn all-package-names [{:keys [packages] :as db}]
  (map first packages))

(defn all-regexes-in
  ([db]
   (all-regexes-in db (all-package-names db)))
  ([{:keys [packages] :as db} package-names]
   (into (sorted-set)
         (for [pname package-names
               [k _] (get packages pname)]
           [pname k]))))

(defn automaton-at [{:keys [packages] :as db} path]
  (get-in packages (conj path :automaton)))

#_(defn fast-automaton-at [path]
  (get-in packages (conj path :fast-automaton)))

(defn regex-str-at [{:keys [packages] :as db} path]
  (get-in packages (conj path :regex-str)))

(defn source-str-at [{:keys [packages] :as db} path]
  (get-in packages (conj path :source-str)))

(defn package-stats [{:keys [packages] :as db}]
  (let [num-packages (count packages)
        num-automata (count (into []
                                  (mapcat second)
                                  packages))]
    (println
     (str/join "\n"
               [(str "num-packages = " num-packages)
                (str "num-automata = " num-automata)]))))

(defn all-packages [{:keys [packages] :as db}]
  packages)

(defn match-one [db s path]
  (let [a (automaton-at db path)]
    (every? #(automaton/match? a %) s)))

(defn matching-children [db s path]
  (let [children (loom.graph/successors (:g db) path)]
    (filter (partial match-one db s) children)))

(defn horizon [lat s]
  ;; no need to keep a "seen" list since this is a dag
  (if-not (seq s)
    #{}
    (loop [worklist #{TOP} horizon #{}]
      (if-let [path (first worklist)]
        (if-let [cs (seq (matching-children lat s path))]
          (recur (-> worklist
                     (disj path)
                     (into cs))
                 horizon)
          (recur (disj worklist path)
                 (conj horizon path)))
        horizon))))

(defn lt
  "Lattice proper subset"
  [{:keys [g :as db]} path1 path2]
  (when-let [sp (loom.alg/shortest-path g path2 path1)]
    #_(println "st::shortest-path" sp)
    true))

(defn gt
  "Lattice proper superset"
  [db path1 path2]
  (lt db path2 path1))

(defn lte
  "Lattice subseteq"
  [db path1 path2]
  (or (= path1 path2)
      (lt db path1 path2)))

(defn gte
  "Lattice superseteq"
  [db path1 path2]
  (or (= path1 path2)
      (gt db path1 path2)))

(defn comparable? [db path1 path2]
  (or (lt db path1 path2)
      (gt db path1 path2)
      (= path1 path2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stage 0: Compute Pairwise Subset Relation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn spit-headers
  "Write 0-based index for all automata to a header file."
  [db result-dir]
  (let [re-paths (all-regexes-in db)]
    (spit (os/->file result-dir "header")
          (with-out-str
            (fipp/pprint
              (into (sorted-map)
                    (map-indexed #(vector %1 %2))
                    re-paths))))))

(defn compute-subsets
  [db]
  (let [re-paths (all-regexes-in db)
        re-pairs (for [p1 re-paths
                       p2 re-paths
                       :when (not (identical? p1 p2))]
                   [p1 p2])
        f (fn [[p1 p2]]
            (let [a1 (automaton-at db p1)
                  a2 (automaton-at db p2)]
              (vector
                ;; (hash p1)
                ;; (hash p2)
                p1
                p2
                (automaton/subset? a1 a2))))]
    (map f re-pairs)))

(defn spit-subsets
  [db result-dir]
  (let [num-regexes (count (all-regexes-in db))
        num-pairs (long (- (Math/pow num-regexes 2) num-regexes))
        *bar (atom (pr/progress-bar num-pairs))]
    (with-open [f (-> (os/->file result-dir "subsets")
                      (io/writer)
                      (PrintWriter.))]
      (doseq [x (compute-subsets db)]
        (.println f (pr-str x))
        (swap! *bar pr/tick)
        (when (zero? (mod (:progress @*bar) 20000))
          (pr/print @*bar))))
    (pr/print (pr/done @*bar))))

(defn reverse-map [m]
  (into (empty m)
        (for [[k v] m]
          [v k])))

(defn read-subsets
  [db result-dir]
  (let [decode (read-string (slurp (os/->file result-dir "header")))
        encode (reverse-map decode)
        num-rows (count decode)
        m (matrix/zero-matrix num-rows num-rows)]
    (with-open [in (PushbackReader. (io/reader (os/->file result-dir "subsets")))]
      (loop [in in]
        (if-let [[p1 p2 b] (read {:eof nil} in)]
          (let [x (encode p1)
                y (encode p2)]
            (matrix/mset! m x y (if b 1 0))
            (recur in))
          (assoc db
                 :m m
                 :encode encode
                 :decode decode))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stage 1: Remove equivalences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- uf-classes
  "Return a seq of all equivalence classes from union-find data structure. Each equivalence class is a set."
  [uf]
  (->>
   (vals (group-by uf (keys (.elt-map uf))))
   (map #(apply sorted-set %))))

(defn compute-equivalences
  "Return all equivalence classes as a seq of equivalence classes, where each equivalence class is a set of indices."
  [{:keys [encode decode m] :as db}]
  (let [[x-max y-max] (matrix/shape m)]
    (loop [i 0 equivs (apply uf/union-find (range x-max))]
      (if (>= i x-max)
        (uf-classes equivs)
        (recur (inc i)
               (loop [j i equivs equivs]
                 (if (>= j y-max)
                   equivs
                   (let [equiv? (and (= 1.0 (matrix/mget m i j))
                                     (= 1.0 (matrix/mget m j i)))]
                     (recur (inc j)
                            (if equiv?
                              (uf/union equivs i j)
                              equivs))))))))))

(defn row-view [m i]
  (matrix/select-view m i :all))

(defn col-view [m j]
  (matrix/select-view m :all j))

(defn remove-equivalences [{:keys [encode decode m] :as db}]
  (let [equivalences (compute-equivalences db)]
    (doseq [e equivalences
            i (next e)] ;; the first element of each equivalence class is the canonical element, so keep it
      ;; remove all outgoing edges from i
      (let [r (row-view m i)]
        (matrix/fill! r 0))
      ;; remove all incoming edges to i
      (let [c (col-view m i)]
        (matrix/fill! c 0)))
    (assoc db
           :equivalences equivalences
           :canonical (into (sorted-set)
                            (map first)
                            equivalences))))

(defn is-canonical? [{:keys [canonical] :as db} idx]
  (contains? canonical idx))

(defn is-decoded-canonical? [{:keys [canonical encode] :as db} path]
  (contains? canonical (encode path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stage 2: Transitive Reduction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn transitive-reduction-edges [{:keys [m] :as db}]
  ;; Explanation: m is a transitively closed DAG by construction.  m2 consists
  ;; of all paths in m of length 2 (or more, since m is transitively closed).
  ;; In other words, all of m2's edges are redundant since they are
  ;; compositions of paths from m.  Thus, the transitive reduction must have
  ;; only those edges in m, but not in m2. It is crucial that m is transitively
  ;; closed to start with. Otherwise we would have to first compute the
  ;; transitive closure of m.
  (let [m2 (matrix/mmul m m)
        [x-max y-max] (matrix/shape m)]
    (for [i (range x-max)
          j (range y-max)
          :when (and (> (matrix/mget m i j) 0.0)
                     (= (matrix/mget m2 i j) 0.0))]
      [i j])))

(defn remove-transitive-edges [{:keys [m] :as db}]
  (let [[x-max y-max] (matrix/shape m)
        ;; the following doall is important! Otherwise the matrix won't get initialized
        kept-edges (doall (transitive-reduction-edges db))]
    #_(println (count kept-edges) "retained edges")
    (matrix/fill! m 0.0)
    (doseq [[i j] kept-edges]
      (matrix/mset! m i j 1.0))
    db))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn loom-graph
  "Create a loom graph from matrix data. Do not do this for large graphs, as it will definitely choke."
  [{:keys [decode encode m] :as db}]
  (let [[x-max y-max] (matrix/shape m)
        adj-list
        (apply concat
               (for [i (range x-max) j (range y-max)]
                 (if (> (matrix/mget m i j) 0.0)
                   (list [(decode j) (decode i)])
                   nil)))
        g (apply loom.graph/digraph adj-list)]
    (apply loom.graph/add-nodes g
           ;; only keep the canonical elements, since noncanonical ones have had all their edges removed
           (filter (partial is-decoded-canonical? db) (keys encode)))))

(defn add-loom-graph [db]
  (let [g (loom-graph db)]
    (assoc db :g g)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visualization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dot-graph
  "Create a .dot file from graph data. Do not do this for large graphs, as it will definitely choke."
  [db]
  (loom.io/dot (:g db) "db.dot"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graph Analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn keep-only-nodes* [g nodes]
  (let [orig-nodes (set (loom.graph/nodes g))
        delete-me (set/difference orig-nodes (set nodes))]
    (loom.graph/remove-nodes* g delete-me)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Entry Point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- compress-db
  [db]
  {:post [(s/validate compact-db-schema %)]}
  (select-keys db [:packages :g]))

(defn- run-rdag-flow
  [db result-dir]
  {:post [(s/validate full-db-schema %)]}
  (spit-headers db result-dir)
  (log/info "Computing subset relation")
  (spit-subsets db result-dir)
  (log/info "Building RDAG")
  (-> db
      (read-subsets result-dir)
      (remove-equivalences)
      (remove-transitive-edges)
      (add-loom-graph)))

(defn create-db-from-scratch
  [package-dir output-file]
  {:post [(s/validate compact-db-schema %)]}
  (let [result-dir (os/make-temp-dir (os/temp-directory) "parsimony")
        db (-> (init-db package-dir)
               (run-rdag-flow result-dir)
               (compress-db))]
    (log/info "Serializing RDAG")
    (serialize db (os/->file output-file))
    (doseq [f (file-seq (os/->file result-dir))]
      (.deleteOnExit f))
    db))

(defn restore-db [db-path]
  {:post [(s/validate compact-db-schema %)]}
  (compress-db (deserialize (os/->url db-path))))

