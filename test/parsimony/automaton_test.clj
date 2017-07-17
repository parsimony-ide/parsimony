(ns parsimony.automaton-test
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.test :as t :refer [deftest is testing]]
            [ubergraph.core :as uber]
            [parsimony.common-test :refer [is-not]]
            [parsimony.rdag.automaton :as automaton]
            [parsimony.rdag.automaton :refer :all]
            [parsimony.rdag.compiler :as compiler]
            [parsimony.rdag.syntax :refer [source->brics]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Raw BRICS Input Strings
;; - This stuff just tests the edge cases in the BRICs syntax itself
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn gen-raw [s]
  (str->automaton s))

(defmacro check-raw [regexp-str test-input]
  `(let [a# (str->automaton ~regexp-str)]
     (is (match? a# ~test-input))))

(defmacro check-neg-raw [regexp-str test-input]
  `(let [a# (str->automaton ~regexp-str)]
     (is (not (match? a# ~test-input)))))

(deftest raw-automaton
  ;; basic
  (check-raw "a" "a")
  (check-raw "abc" "abc")
  (check-raw "123" "123")
  (check-raw "今天" "今天") ;; unicode
  ;; escaped non-reserved chars
  (check-raw "\\a" "a")
  (check-neg-raw "\\a" "b")
  (check-raw "\\x" "x")
  ;; catchall
  (check-raw "." "a")
  (check-raw "." (str \newline))
  (check-raw "." (str \formfeed))
  (check-raw "." (str \backspace))
  (check-raw "." (str \tab))
  ;; unescaped dash
  (check-raw "-" "-")
  (check-neg-raw "-" " ")
  ;; dash inside charclass
  (check-raw "[-]" "-")
  ;; escaped dot
  (check-raw "\\." ".")
  (check-neg-raw "\\." "a")
  ;; unescaped lsquarebrace is not ok
  (is (thrown? IllegalArgumentException (str->regexp "[")))
  ;; escaped lsquarebrace
  (check-raw "\\[" "[")
  (check-neg-raw "\\[" " ")
  ;; unescaped rsquarebrace is ok
  (check-raw "]" "]")
  (check-neg-raw "]" " ")
  ;; escaped rsquarebrace
  (check-raw "\\]" "]")
  (check-neg-raw "\\]" " ")
  ;; empty language
  (check-neg-raw "#" "")
  (check-neg-raw "#" "a")
  (check-neg-raw "#" "ab")
  ;; any string
  (check-raw "@" "")
  (check-raw "@" "rcgcgfy,.p")
  (check-raw "@" "我不是")
  (check-raw "@" (str \newline \space \tab \formfeed \backspace \return)) ;; weird characters
  ;; enclosed string with no reserved chars
  (check-raw (str \" "string contents" \") "string contents")
  ;; enclosed string with reserved chars
  (check-raw (str \" "@" \") "@") ;; @ matches literal @ instead of any string, since dquotes escape
  (check-neg-raw (str \" "@" \") " ")
  ;; numerical interval
  (doseq [i (range 10)]
    (check-raw "<0-9>" (str i)))
  (check-neg-raw "<0-9>" "10")
  (check-raw "<00-99>" "01")
  (check-neg-raw "<00-99>" "1") ;; matches only if input has same number of digits
  ;; basic charclass
  (check-raw "[a-z]" "a")
  (check-raw "[a-z]" "f")
  ;; charclass with reserved chars
  (check-raw "[\\^]" "^") ;; escaped caret at the beginning is literal ^
  (check-neg-raw "[\\^]" "a")
  (check-raw "[ ^]" "^") ;; ^ not in first position inside charclass is considered literal ^, not charclass negation
  (check-raw "[^^]" "a") ;; ^ after initial negation metachar, ^ is considered literal
  (check-neg-raw "[^^]" "^")
  (is (thrown? IllegalArgumentException (str->regexp "[]"))) ;; empty positive charclass not allowed
  (is (thrown? IllegalArgumentException (str->regexp "[^]"))) ;; empty negative charclass not allowed
  (check-raw "[(]" "(")
  (check-raw "[)]" ")")
  (check-raw "[{]" "{")
  (check-raw "[}]" "}")
  (check-raw "[[]" "[")
  (check-raw "[]]" "]") ;; even close rbrace inside charclass does not need to be escaped!
  (check-raw "[?]" "?")
  (check-raw "[*]" "*")
  (check-raw "[+]" "+")
  (check-raw "[.]" ".")
  (check-raw "[#]" "#")
  (check-raw "[@]" "@")
  (check-raw (str \[ \" \]) (str \")) ;; the quotes do not have backslashes before them. that's a clojure escape
  (check-raw (str \[ \' \]) (str \'))
  (check-raw "[\\\\]" "\\") ;; backslash must be escaped inside charclass (raw string is two backslashes in a row, not four)
  (check-raw "[\\-]" "-") ;; dash must be escaped inside charclass

  (is (thrown? IllegalArgumentException (str->regexp "(")))
  (check-raw ")" ")") ;; unescaped close paren is ok
  (check-raw "\\)" ")") ;; but escaped is ok too
  ;; unicode strings
  (check-raw "\\u0003" "u0003") ;; unicode code point encoding (i.e., \u0003) do NOT work here
  (check-neg-raw "\\u0003" "0") ;; unicode code point encoding (i.e., \u0003) do NOT work here
  ;; whitespace
  (check-raw "\n" "\n")
  (check-raw "\t" "\t")
  (check-raw "\r" "\r")
  (check-raw " " " ")
  (check-neg-raw " " "")
  (check-neg-raw "\\n" "\n") ;; does not accept usual escaped forms like \n, \t, ...
  (check-raw "\\\\n" "\\n")
  (check-raw "\\n" "n") ;; \n is the same as n to brics
  (check-raw "\n|\t|\r" "\n")
  (check-raw "\n|\t|\r" "\t")
  (check-raw "\n|\t|\r" "\r")
  (check-neg-raw "\n|\t|\r" " ")
  (check-raw "(\n|\t|\r)+" "\n")
  (check-raw "(\n|\t|\r)+" "\n\n")
  ;; open parentheses
  (check-raw "\\(" "(")
  ;; pipe
  (check-raw "\\|" "|")
  (check-raw "|" "|")
  ;; angle brackets
  (check-raw ">" ">")
  (check-raw "\\<" "<")
  (is (thrown? IllegalArgumentException (check-raw "<" "<"))) ;; can't have bare open angle bracket
  ;; double quotes
  (check-raw "\\\"" "\"")  ;; double quotes need to be escaped when outside of charclass
  (check-raw "[\"]" "\"")  ;; but not when inside a charclass
  ;; single quotes
  (check-raw "'" "'") ;; single quotes do not need to be escaped
  ;; negation
  (check-neg-raw "~a" "a")
  (check-raw "~a" "b")
  (check-neg-raw "~ab" "ab")
  (check-neg-raw "a~(b+)c" "abbbc")
  (check-raw "a~(b+)c" "ac")
  (check-raw "a~(bb+)c" "abc")
  (check-neg-raw "a~(bb+)c" "abbc")
  (check-raw "a~bc" "ac")
  (check-raw "a~bc" "abbc")
  (check-raw "a~bc" "abbbbbbc")
  (check-neg-raw "a~(b*)c" "ac")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Normalized Expressions
;; - This tests the normalizer in parsimony.rdag.syntax to make sure it outputs
;;   regular expressions as expected by BRICs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn gen [s]
  (str->automaton (source->brics s)))

(deftest automaton
  (is (= (.getStrings (gen "a | b | c") 1) #{"a" "b" "c"}))
  (is (= (.getStrings (gen "[abc]{1}") 1) #{"a" "b" "c"}))
  (is (= (.getStrings (gen "[abc]{1,3}") 2) #{"aa" "ab" "ac" "ba" "bb" "bc" "ca" "cb" "cc"}))
  (is (= (.getStrings (gen "[abc]{1,3}") 4) #{})))

(deftest matching
  (let [a (optimize (str->automaton "a+"))]
    (is (match? a "a"))
    (is (match? a "aa"))
    (is (match? a "aaaa")))

  (let [a (optimize (str->automaton "[ ]+"))]
    (is (match? a " "))
    (is (match? a "  ")))

  (let [a (optimize (str->automaton "\\\\"))]
    (is (match? a "\\"))))

(deftest whitespace
  (let [a (gen "([ ] | \\t | \\r | \\n)+")]
    (is (match? a " "))
    (is (match? a "  "))
    (is (not (match? a "")))
    (is (match? a "\n"))
    (is (match? a "\t"))
    (is (match? a "\r"))
    (is (match? a "\n\n\n\t\r\n"))))

(deftest lparen
  (let [a (gen "\\(")]
    (is (match? a "("))))

(deftest pipe
  (let [a (gen "\\|")]
    (is (match? a "|"))))

(deftest unicode
  (let [a (gen "[\\u00C0-\\u00D6]")]
    (is (match? a (str (char 0x00c0))))
    (is (match? a (str (char 0x00d6))))
    (is (not (match? a (str (char 0x00d7)))))))

;; from antlr4 grammar
(deftest big-unicode
  (let [a (gen "([A-Z] | [a-z] | [\u00C0-\u00D6] | [\u00D8-\u00F6] | [\u00F8-\u02FF] | [\u0370-\u037D] | [\u037F-\u1FFF] | [\u200C-\u200D] | [\u2070-\u218F] | [\u2C00-\u2FEF] | [\u3001-\uD7FF] | [\uF900-\uFDCF] | [\uFDF0-\uFFFD]) ([A-Z] | [a-z] | [\u00C0-\u00D6] | [\u00D8-\u00F6] | [\u00F8-\u02FF] | [\u0370-\u037D] | [\u037F-\u1FFF] | [\u200C-\u200D] | [\u2070-\u218F] | [\u2C00-\u2FEF] | [\u3001-\uD7FF] | [\uF900-\uFDCF] | [\uFDF0-\uFFFD] | [0-9] | _ | \u00B7 | [\u0300-\u036F] | [\u203F-\u2040])*")]
    (is (not (match? a "")))
    (is (match? a (str (char 0x00c0))))
    (is (match? a (str (char 0x00c0)
                       (char 0x00d1)
                       (char 0x00d2))))))

(deftest equality
  (is (= (str->automaton "abc") (str->automaton "abc")))
  (is (= (str->automaton "a|b|c") (str->automaton "[abc]")))
  (is (= (str->automaton "..*") (str->automaton ".+"))))

(deftest lattice
  (let [a (str->automaton "abc")
        b (str->automaton "[a-z]+")]
    (is (match? a "abc"))
    (is (match? b "abcde"))
    (is (subset? a b))
    (is (not (subset? b a)))))

(deftest negcharclass-star
  (let [s "[^a]*"
        re (str->regexp (source->brics s))
        a (regexp->automaton re)]
    #_(println re)
    #_(println (.toAutomaton re))
    (is (= (str re) "((.&~(\\a)))*")) ;; <- note the intersection used to encode single-char constraint
    (is (match? a "b"))
    (is (match? a "bcdef"))
    (is (match? a ""))
    (is-not (match? a "a"))
    (is-not (match? a "a a"))))

;; experimenting with behavior of negation and intersection
(deftest negate-intersection-1
  ;; with intersection
  (check-raw "a(~a&.)a" "aba")
  (check-neg-raw "a (~a&.) a" "aaba")
  ;; without intersection
  (check-raw "a~aa" "aba")
  (check-raw "a~aa" "aaba") ;; unintuitive: ab matches ~a
  (check-neg-raw "~(abc)" "abc")
  (check-neg-raw "~(abc)def" "abcdef")
  (check-raw "~(abc).*" "abc") ;; unintuitive: matches because ~(abc) can match a or ab
  (check-neg-raw "(~(abc) & ...).*" "abc") ;; intuitive
  (check-raw "(~(abc)&...).*" "babc") ;; intuitive: bab matches prefix, c matches rest
  (check-neg-raw "b~(a+)b" "bab")
  (check-neg-raw "b~(a+)b" "baab")
  (check-raw "b~(a+)b" "bb")
  (check-raw "b~(a+)b" "babb")
  (check-neg-raw "b~(a*)b" "bb") ;; unintuitive: a* matches empty string between b and b
  (check-neg-raw "b~(a*)b" "bab")
  (check-raw "b~((ab)+)b" "bb")
  (check-neg-raw "b~((ab)+)b" "babb")
  (check-raw "b~((ab)+)b" "bcb"))

;; problem cases:
;; 1. negation wrapped in repetition
;; 2. negation followed by repetition
;; 3. repetition inside negation
;; 4. union inside negation
(deftest negate-problems
  ;; problem case 1
  (check-raw "(~a)*" "aa")         ;; problem: aa matches ~a, so entire parse is read "aa matches one repetition of ~a"
  (check-raw "((~a)*&.*).*" "aab") ;; even if we use the intersection trick, the problem persists: here, aab matches the prefix
  ;; fix: intersection with . ensures that each repetition is one char
  (check-neg-raw "(~a&.)*" "aa")

  ;; problem case 2
  (check-raw "~a.+" "aa")   ;; problem: ~a matches epsilon, .+ matches aa
  (check-raw "~a.*" "aa")   ;; problem: ~a matches epsilon, .* matches aa, OR ~a matches aa, .* matches epsilon
  (check-raw "~aa.*" "aa") ;; the problem exists even if there is something between the negation and repetition
  ;; fix: intersection with . ensures that each repetition is one char
  (check-neg-raw "(~a&.).+" "aa")
  (check-neg-raw "(~a&.).*" "a")
  (check-neg-raw "(~a&.).*" "aa")

  ;; combination of problem cases 1 and 2 is also bad
  (check-neg-raw "~(a*).*" "aa")
  (check-raw "~(a+).*" "aa") ;; problem: this gets compiled down to the .* automaton

  ;; problem case 4
  (check-raw "~(aa|aaa).*" "aaa") ;; aaa matches .*
  (check-neg-raw "(~(aa)&.{2})|(~(aaa)&.{3}).*" "aaa"))

(deftest bang-negation
  (is (match? (gen "!a") "b"))
  (is-not (match? (gen "!a") "a"))
  (is-not (match? (gen "!a+") "a"))
  (is-not (match? (gen "!a+") "aa"))
  (is-not (match? (gen "!a+") "aab"))
  (is (match? (gen "!a+") "bbb"))
  (is-not (match? (gen "![a-z]+") "c"))
  (is (match? (gen "![a-z]+") "019238~")))

;; how to deal with nested comments using raw BRICs regexes
(deftest raw-comments
  ;; bad: the extra */ is matched by .*
  (check-raw "/\\*.*\\*/" "/* */ */")
  ;; fix: intersect with a negate mask that does not allow */ on the inside
  (check-raw     "/\\*.*\\*/&~(/\\*.*\\*/.*\\*/)" "/* */")
  (check-neg-raw "/\\*.*\\*/&~(/\\*.*\\*/.*\\*/)" "/* */ */"))

(deftest neg-intersect
  ;; how to deal with nested comments using neg-intersect
  (is-not (match? (gen "/\\*.*\\*/ !& /\\*.*\\*/.*\\*/") "/* */ */"))
  (is-not (match? (gen "/\\*.*\\*/ !& /\\*.*\\*/.*\\*/") "/* /* */ */"))
  (is-not (match? (gen "/\\*.*\\*/ !& /\\*.*\\*/.*\\*/") "/*"))
  (is-not (match? (gen "/\\*.*\\*/ !& /\\*.*\\*/.*\\*/") "*/"))
  (is-not (match? (gen "/\\*.*\\*/ !& /\\*.*\\*/.*\\*/") "/*/"))
  (is (match? (gen "/\\*.*\\*/ !& /\\*.*\\*/.*\\*/") "/**/"))
  (is (match? (gen "/\\*.*\\*/ !& /\\*.*\\*/.*\\*/") "/* stuff inside */"))
  (is (match? (gen "/\\*.*\\*/ !& /\\*.*\\*/.*\\*/") "/* stuff inside\n\r\nmore\t */")))

(deftest sparql
  ;; Note: this used to throw an exception in BRICS because unicode \u0022 corresponds to a double quote ", which was
  ;; then translated in xform.brics to a bare " instead of an escaped quote \"
  (let [s "\\\" (!(\\u0022 | \\u005C | \\u000A | \\u000D) | \\\\ (t | b | n | r | f | \\\" | \\'))* \\\""]
    (is (gen s))))

(deftest clojure-symbol
  (testing "components"
    (is-not (match? (gen "\\.") " "))
    (is (match? (gen "\\.") "."))

    (let [source-str "!([0-9] | \\^ | ` | \\' | \\\" | \\# | \\~ | \\@ | : | / | % | \\( | \\) | \\[ | \\] | \\{ | \\} | [ \\n\\r\\t\\,])"
          a (gen source-str)]
      (is (match? a "a"))
      (is-not (match? a "`"))
      (is-not (match? a "0"))
      (is-not (match? a " "))
      (is-not (match? a ","))
      (is-not (match? a "aa"))
      (let [b-str (str "(" source-str ")*")
            b (gen b-str)]
        #_(println (source->brics b-str))
        (is (match? b "a"))
        (is (match? b "aa") "should match repetitions")
        (is-not (match? b "a   a") "should not match spaces")
        )))

  (testing "full definition"
    (let [a (gen "\\. | / | !([0-9] | \\^ | ` | \\' | \\\" | \\# | \\~ | \\@ | : | / | % | \\( | \\) | \\[ | \\] | \\{ | \\} | [ \\n\\r\\t\\,]) (!([0-9] | \\^ | ` | \\' | \\\" | \\# | \\~ | \\@ | : | / | % | \\( | \\) | \\[ | \\] | \\{ | \\} | [ \\n\\r\\t\\,]) | [0-9] | \\.)* (: (!([0-9] | \\^ | ` | \\' | \\\" | \\# | \\~ | \\@ | : | / | % | \\( | \\) | \\[ | \\] | \\{ | \\} | [ \\n\\r\\t\\,]) | [0-9] | \\.)+)*")]
      (is (match? a "."))
      (is (match? a "/"))
      (is-not (match? a " "))
      (is-not (match? a "a b c   ") "Should not match whitespace"))))

(deftest abnf
  (let [a (gen "\\< !\\>* \\>")]
    (is (match? a "<>"))
    (is (match? a "< >"))
    (is (match? a "<xyz>"))
    #_(pprint (take 10 (random-strings a 10)))))

(deftest webidl
  (let [a (gen "![\\t\\n\\r 0-9A-Z_a-z]")]
    (is-not (match? a "12"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BRICS->CLJS Conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; uncomment to see converted automaton
#_(deftest brics->cljs
  (let [a (str->automaton "a")]
    (pprint (automaton->js-automaton a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automaton Graphs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce a1 (str->automaton "(abc|def)+de+fg+[xyz][123]"))
(def g1 (automaton->graph a1))

(defonce a2 (str->automaton "[a-z]+[0-9]*"))
(def g2 (automaton->graph a2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example String Generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; uncomment to see random strings
#_(deftest test-random-strings
  (pprint (take 10 (random-strings a1 10)))
  (pprint (take 10 (random-strings a2 5))))
