(ns parsimony.rdag.xform.common
  (:require [loom.graph :refer [digraph]]
            [loom.alg :refer [dag? topsort]]
            [parsimony.common :refer [vec-join]]
            ))

(defrecord Named [ident])

(def ast-transform
  {:S identity
   :empty-lines (constantly {})
   :line-comment (constantly {})
   :production (fn [id re]
                 {(keyword id) re})
   :regexp flatten
   :unionexp (fn [& xs]
               (vec-join "|" xs))
   :interexp identity
   :neginterexp (fn [& xs]
                  (vec-join "!&" xs))
   :posinterexp (fn [& xs]
                  (vec-join "&" xs))
   :concatexp (fn [& xs]
                (vec xs))
   :repeatexp identity
   :optionalexp #(vector % "?")
   :starexp #(vector % "*")
   :plusexp #(vector % "+")
   :nexp (fn [x n]
           (vector x "{" n "}"))
   :nplusexp (fn [x n]
               (vector x "{" n ",}"))
   :nmexp (fn [x n m]
            (vector x "{" n "," m "}"))
   :complexp identity
   :charclassexp identity
   :poscharclassexp #(vector "[" % "]")
   :negcharclassexp #(vector "[^" % "]")
   :charclasses (fn [& args]
                  (vec args))
   :charclass identity
   :charclassrange (fn [x y]
                     (vector x "-" y))
   :simpleexp identity
   :groupexp #(vector "(" % ")")
   :namedexp #(Named. %)
   :charexp identity
   :cc-charexp identity
   :cc-nonreservedcharexp identity
   :nonreservedcharexp identity
   :anychar identity
   :num identity
   :ident identity})

(defn unresolved-syms [ast]
  (keep (fn [x]
          (when (instance? Named x)
            (keyword (:ident x))))
        ast))

(defn dep-graph [asts]
  (let [edges (reduce
               (fn [acc [ident ast]]
                 (if-let [syms (seq (unresolved-syms ast))]
                   (into acc
                         (map vector (repeat ident) syms))
                   (conj acc [ident 0])))
               []
               asts)]
    (apply digraph edges)))

(defn substitute-one
  "Replace all named references within this-ast, using current expressions stored in asts"
  [this-ast asts]
  (mapcat (fn [x]
            (if (instance? Named x)
              (get asts (keyword (:ident x)))
              [x]))
          this-ast))

(defn substitute-all
  "Replace all named references with their (sub-)expressions"
  [asts]
  (let [deps (dep-graph asts)]
    (when (dag? deps)
      (let [order (reverse (topsort deps))]
        (reduce
         (fn [acc x]
           (if-let [ast (get acc x)]
             (assoc acc x (apply str (substitute-one ast acc)))
             acc))
         asts
         order)))))
