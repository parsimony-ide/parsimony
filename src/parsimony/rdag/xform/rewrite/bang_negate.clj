(ns parsimony.rdag.xform.rewrite.bang-negate
  (:require [parsimony.rdag.tree-visitor :refer [reverse-preorder-visit skip-leaves on]]))

(defn- edit []
  (fn [node state]
    {:node [:groupexp
            [:posinterexp
             (assoc node 0 :tildeexp)
             [:simpleexp "."]]]}))

(defn bang-negate
  "Rewrite ! to intersection with a ~ negation"
  [ast]
  (:node (reverse-preorder-visit ast nil
                                 [(skip-leaves)
                                  (on #{:negateexp})
                                  (edit)])))
