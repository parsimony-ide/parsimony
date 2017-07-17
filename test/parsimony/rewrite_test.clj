(ns parsimony.rewrite-test
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test :as t :refer [deftest is]]
            [instaparse.core :as insta]
            [parsimony.rdag.syntax :refer [regexp-parser ast->brics source->brics]]
            [parsimony.rdag.xform.brics :as brics]
            [parsimony.rdag.xform.rewrite.neg-intersect :refer [neg-intersect]]
            [parsimony.rdag.xform.rewrite.bang-negate :refer [bang-negate]]
            [parsimony.rdag.xform.rewrite.flatten-union :refer [remove-extraneous-levels flatten-union]]))

(defn- no-rewrite-brics
  "Return BRICS string representation of this ast WITHOUT doing rewrites. We don't want to run rewrites since we're
  doing the rewrites explicitly in the tests themselves"
  [ast]
  (apply str (insta/transform brics/ast-transform ast)))

(deftest remove-extraneous-levels-1
  (is (= [:regexp [:charexp [:nonreservedcharexp "a"]]] (remove-extraneous-levels (regexp-parser "a"))))
  (is (= [:regexp [:concatexp
                   [:charexp [:nonreservedcharexp "a"]]
                   [:charexp [:nonreservedcharexp "b"]]]] (remove-extraneous-levels (regexp-parser "ab")))))

(deftest flatten-union-1
  (is (= "a|b|c|d" (ast->brics (flatten-union (regexp-parser "a|b|(c|d)"))))))

(deftest bang-negate-1
  (is (= "(~a&.)" (no-rewrite-brics (bang-negate (regexp-parser "!a")))))
  (is (= "(~(a|b)&.)" (no-rewrite-brics (bang-negate (regexp-parser "!(a|b)")))))
  (is (= "(~(a|b|[a-z0-9])&.)" (no-rewrite-brics (bang-negate (regexp-parser "!(a|b|[a-z0-9])"))))))

(deftest neg-intersect-1
  (is (= "(a&~(b))" (no-rewrite-brics (neg-intersect (regexp-parser "a !& b")))))
  (is (= "(a&~(bc))" (no-rewrite-brics (neg-intersect (regexp-parser "a !& bc")))))
  (is (= "(abcd&~(bcd))" (no-rewrite-brics (neg-intersect (regexp-parser "abcd !& bcd")))))
  )

(deftest full-rewrites-1
  (is (= "(~a&.)&((b&~(c)))" (source->brics "!a&(b!&c)")))
  (is (= "(b&~(((~a&.))))" (source->brics "b!&(!a)"))))
