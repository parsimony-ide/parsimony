(ns parsimony.rdag.xform.rewrite.neg-intersect
  (:require [parsimony.rdag.tree-visitor :refer [reverse-preorder-visit skip-leaves on]]))

(defn- edit []
  (fn [[_ x y] state]
    {:node [:groupexp
            [:posinterexp
             x
             [:tildeexp [:groupexp y]]]]}))

(defn neg-intersect
  "Rewrite !& to intersection with a ~ negation"
  [ast]
  (:node (reverse-preorder-visit ast nil
                                 [(skip-leaves)
                                  (on #{:neginterexp})
                                  (edit)])))
