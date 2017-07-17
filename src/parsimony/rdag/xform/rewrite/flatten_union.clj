(ns parsimony.rdag.xform.rewrite.flatten-union
  (:require [instaparse.core :as insta]
            [parsimony.rdag.tree-visitor :refer [reverse-preorder-visit skip-leaves on]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remove-extraneous-levels
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -extract-singleton
  [kw & children]
  (if (= 1 (count children))
    (first children)
    (into [kw] children)))

(defn remove-extraneous-levels
  "Given a concrete parse tree, remove all extraneous levels that add no semantic information"
  [ast]
  (let [xform {:unionexp (partial -extract-singleton :unionexp)
               :interexp (partial -extract-singleton :interexp)
               :neginterexp (partial -extract-singleton :neginterexp)
               :posinterexp (partial -extract-singleton :posinterexp)
               :concatexp (partial -extract-singleton :concatexp)
               :repeatexp identity
               :complexp identity
               :charclassexp identity
               :simpleexp identity
               :groupexp identity}]
    (insta/transform xform ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flatten-union
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -flatten
  "Remove one level of nesting with respect to subtrees of type kw"
  [kw & children]
  (letfn [(f [ast]
            (if (= kw (first ast))
              (next ast)
              [ast]))]
    (into [kw]
          (mapcat f)
          children)))

(defn- -distribute-concats
  "Distributes concatenation across unions"
  [[_ & children]]
  (loop [partials [[]] children children]
    (if-let [c (first children)]
      (case (first c)
        :unionexp
        (recur (into []
                     (mapcat (fn [p]
                               (for [cc (next c)]
                                 (conj p cc))))
                     partials)
               (next children))
        (recur (into [] (map #(conj % c)) partials)
               (next children)))
      (if (> (count partials) 1)
        (into [:unionexp]
              (map #(into [:concatexp] %))
              partials)
        ;; do not introduce extra level of :unionexp unless necessary, because it blocks subsequent flattening
        (into [:concatexp] (first partials))))))

(defn flatten-union
  "Given a tree with arbitrarily nested unions and concatenations, produce a tree with a single top-level union and
  child concatenations"
  [ast]
  (let [xform
        {:concatexp (comp -distribute-concats
                          (partial -flatten :concatexp))
         :unionexp (partial -flatten :unionexp)}]
    ;; loop to fixpoint because distributing can introduce more opportunities to flatten
    (loop [ast (remove-extraneous-levels ast) last-ast nil]
      (if (= ast last-ast)
        ast
        (recur (insta/transform xform ast) ast)))))
