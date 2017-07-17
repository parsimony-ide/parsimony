(ns parsimony.rdag.xform.brics
  "Conversion of Parsimony AST to BRICS regular expression input string"
  (:import [parsimony.rdag.xform.common Named])
  (:require [clojure.edn :as edn]
            [instaparse.core :as insta]
            [clojure.pprint :refer [pprint]]
            [parsimony.rdag.tree-visitor :refer [preorder-visit skip-leaves on collect -or -and >n-children]]
            [parsimony.rdag.xform.common :as common]
            [parsimony.rdag.xform.rewrite.bang-negate :refer [bang-negate]]
            [parsimony.rdag.xform.rewrite.neg-intersect :refer [neg-intersect]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semantic Checks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- negation-nodes [ast]
  (:state (preorder-visit ast []
                          [(skip-leaves)
                           (on #{:negateexp})
                           (collect)])))

;; things not allowed inside negations: !,?,*,+,{},&,concat
(defn- illegal-nested-nodes [ast]
  (:state (preorder-visit ast []
                          [(skip-leaves)
                           (-or (on #{:negateexp :optionalexp :starexp :plusexp :nexp :nplusexp :nmexp})
                                (-and (on #{:concatexp})
                                      (>n-children 1))
                                (-and (on #{:interexp})
                                      (>n-children 1)))
                           (collect)])))

(defn- check-illegal-nesting [ast]
  (let [all-failing-nodes
        (into []
              (comp (filter #(seq (illegal-nested-nodes (vec (next %)))))
                    (map #(vector (insta/span %) %)))
              (negation-nodes ast))]
    (when-let [[[i j] failing-node] (first (sort-by ffirst all-failing-nodes))]
      (let [message "Illegally-nested construct. Only union is allowed inside a negation."]
        (throw (ex-info message
                        {:causes #{:illegal-nesting}
                         :message message
                         :index i
                         :ast ast}))))))

(defn semantic-check! [ast]
  (-> ast
      (check-illegal-nesting)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn rewrite [ast]
  (-> ast
      (bang-negate)
      (neg-intersect)))

(defn escaped-char->raw-char
  "These are characters that are escaped in source format, but should be raw in BRICS format"
  [s]
  {:pre [(= 1 (count s))]}
  (case s
    "n" (str \newline)
    "t" (str \tab)
    "r" "\r"
    "f" "\f"
    (str "\\" s)) ;; keep the backslash on everything else (i.e., \< stays \<)
  )

(defn raw-char->escaped-char
  "These are characters that must be escaped (outside of charclasses) in BRICS format"
  [s]
  {:pre [(= 1 (count s))]}
  (case s
    "\\" "\\\\"
    "~" "\\~"
    "!" "\\!"
    "|" "\\|"
    "&" "\\&"
    "?" "\\?"
    "*" "\\*"
    "+" "\\+"
    "\"" "\\\""
    s))

(def ast-transform
  (merge
   common/ast-transform
   {:negateexp #(vector "!" %)
    :tildeexp #(vector "~" %) ;; This construct is added by the bang-negate rewrite rule. It is never part of the initial concrete parse tree.
    :reservedcharexp escaped-char->raw-char
    :unicodecharexp (comp raw-char->escaped-char str edn/read-string)}))

(defn transform [x]
  (semantic-check! x)
  (insta/transform ast-transform (rewrite x)))
