(ns parsimony.syntax-test
  (:import [clojure.lang ExceptionInfo])
  (:require [parsimony.rdag.syntax :refer :all]
            [parsimony.rdag.xform.brics :as xform.brics]
            [parsimony.common-test :refer [is-not]]
            [clojure.pprint :refer [pprint]]
            [clojure.test :as t :refer [deftest is]]
            [instaparse.core :as insta]))

(defn- check-same [s]
  (let [ast (token-rule-parser s :start :production)
        emitted (try (ast->brics ast)
                     (catch Exception e
                       (str "Exception " e)))]
    #_(do
      (println s)
      (pprint ast)
      (println emitted)
      (println "---"))
    (is (= s emitted) (pr-str ast))))

(deftest passing-syntax
  ;; The following tests make no use of optional whitespace within a regexp
  (let [test-inputs
        ["a = b ;"
         "a = b|c ;"
         "a = b|c|d ;"
         "a = b&c ;"
         "a = b&c&d ;"
         "a = b? ;"
         "a = b* ;"
         "a = b+ ;"
         "a = b{1} ;"
         "a = b{1,} ;"
         "a = b{1,2} ;"
         "a = [z] ;"
         "a = [^z] ;"
         "a = [a-z] ;"
         "a = [^a-z] ;"
         "a = . ;"
         "a = @ ;"
         "a = (a) ;"
         "a = abcd ;"

         "a = ab|[a-z]? ;"

         "a = ab|\\x ;"
         "a-b = ab ;"
         ]]
    (doseq [t test-inputs]
      #_(println t)
      (check-same t))))

(deftest failing-syntax
  (let [test-inputs ["^"
                     "&"
                     "?"
                     "*"
                     "+"
                     "{"
                     "}"
                     "["
                     "]"
                     "<"
                     ">"
                     "("
                     ")"
                     "-"
                     ";"
                     "\""
                     "~"
                     "!"]]
    (doseq [t test-inputs]
      (is (thrown? ExceptionInfo (regexp-parser t))))))

(deftest substitution-test
  (let [test-input "
variable = \\? [a-z0-9] <character>+ ;
character = [0-9a-zA-Z_] ; "]
    (is (= (get-in (definition-parser test-input) [:regexes :variable])
           "\\?[a-z0-9][0-9a-zA-Z_]+"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse Tree Checks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest charclass
  (is (thrown? ExceptionInfo (regexp-parser "[]")) "Empty charclass is not allowed")
  (is (regexp-parser "[\"]"))
  (is (regexp-parser "[\\]]")))

(deftest unicode
  (is (regexp-parser "\\u00C0"))
  (is (= (str (char 0x00C0)) (source->brics "\\u00C0")))
  (is (regexp-parser "[\\u00C0-\\u00D6]"))
  #_(println (source->brics "[\\u00C0-\\u00D6]")))

(deftest ast-checks
  (is (thrown? ExceptionInfo (source->brics "!(a?)")))
  (is (thrown? ExceptionInfo (source->brics "!(a*)")))
  (is (thrown? ExceptionInfo (source->brics "!(a+)")))
  (is (thrown? ExceptionInfo (source->brics "!(a{2})")))
  (is (thrown? ExceptionInfo (source->brics "!(a{2,})")))
  (is (thrown? ExceptionInfo (source->brics "!(a{2,3})")))
  (is (thrown? ExceptionInfo (source->brics "!!a")))
  (is (thrown? ExceptionInfo (source->brics "!(abc)")))
  (is (source->brics "!a"))
  (is (source->brics "!(a)"))
  (is (source->brics "!(a|b)"))
  (is (source->brics "!(a|b|(c|d))")))

(deftest intersection
  (is (= "a&b" (source->brics "a&b")))
  (is (= "a&b&c" (source->brics "a&b&c")))
  (is (= "(a&~(b))" (source->brics "a!&b")))
  (is (thrown? ExceptionInfo (source->brics "a!&b!&c") "!& is not associative"))
  (is (thrown? ExceptionInfo (source->brics "a!&b&c") "cannot mix !& and & without grouping"))
  (is (= "((a&~(b)))&c" (source->brics "(a!&b)&c")) "Mixing is ok with parentheses"))

(deftest unicode-escape
  (is (= "\\\"" (source->brics "\\u0022"))))
