(ns parsimony.rdag.syntax
  "Defines the input syntax for creating automata from regular expressions"
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [instaparse.core :as insta]
            [parsimony.rdag.xform.common :as xform.common]
            [parsimony.rdag.xform.brics :as xform.brics]))

(def token-rule-specification
  "
  S                  = empty-lines / line-comment / production
  empty-lines        = #'^\\s*(\\n|\\z)'
  line-comment       = <maybe-ws> <';;'> #'[^\\n]*(\\n|\\z)'
  production         = <maybe-ws> ident <maybe-ws> <sep> <maybe-ws> regexp <maybe-ws> <';'>
  regexp             = unionexp
  unionexp           = interexp (<maybe-ws> <'|'> <maybe-ws> interexp)*
  interexp           = neginterexp | posinterexp
  neginterexp        = concatexp <maybe-ws> <'!&'> <maybe-ws> concatexp
  posinterexp        = concatexp (<maybe-ws> <'&'> <maybe-ws> concatexp)*
  concatexp          = repeatexp (<maybe-ws> repeatexp)*
  repeatexp          = optionalexp | starexp | plusexp | nexp | nplusexp | nmexp | complexp
  optionalexp        = repeatexp <'?'>
  starexp            = repeatexp <'*'>
  plusexp            = repeatexp <'+'>
  nexp               = repeatexp <'{'> <maybe-ws> num <maybe-ws> <'}'>
  nplusexp           = repeatexp <'{'> <maybe-ws> num <maybe-ws> <','> <maybe-ws> <'}'>
  nmexp              = repeatexp <'{'> <maybe-ws> num <maybe-ws> <','> <maybe-ws> num <maybe-ws> <'}'>
  complexp           = negateexp | charclassexp
  negateexp          = <'!'> complexp
  charclassexp       = poscharclassexp | negcharclassexp | simpleexp
  simpleexp          = charexp | '.' | '@' | groupexp | namedexp
  groupexp           = <'('> <maybe-ws> unionexp <maybe-ws> <')'>
  namedexp           = <'<'> ident <'>'>
  poscharclassexp    = <'['> charclasses <']'>
  negcharclassexp    = <'[^'> charclasses <']'>
  charclasses        = charclass+
  charclass          = charclassrange / cc-charexp
  charclassrange     = cc-charexp <'-'> cc-charexp
  charexp            = unicodecharexp / reservedcharexp / nonreservedcharexp
  cc-charexp         = unicodecharexp / reservedcharexp / cc-nonreservedcharexp

  nonreservedcharexp    = #'[^\\~!|&?*+{}\\[\\]()<>^.#@\"\\-;\\n]'
  cc-nonreservedcharexp = #'[^\\\\\\[\\]\\-;\\n]'
  reservedcharexp       = <#'\\\\'> anychar
  unicodecharexp        = #'\\\\u[0-9a-fA-F]{4,6}'

  anychar            = #'.'
  sep                = '='
  num                = #'\\d+'
  ident              = #'[a-zA-Z][a-zA-Z0-9_\\-]*'
  ws                 = #'[ ]+'
  maybe-ws           = #'[ ]*'
  ")

(def token-rule-parser
  (insta/parser token-rule-specification))

(defn parse-or-throw [parser s i & parse-opts]
  (let [ast (apply insta/parse parser (subs s i) parse-opts)
        [_ j] (insta/span ast)]
    (if (insta/failure? ast)
      (throw (ex-info (str "Instaparse failure:" \newline (pr-str ast))
                      {:causes #{:parse-failure}
                       :index i
                       :string (subs s i)
                       :failure ast}))
      ast)))

(defn regexp-parser
  "Parse the given string in Parsimony regular expression syntax"
  [s]
  (parse-or-throw token-rule-parser s 0 :start :regexp))

(defn definition-parser [s]
  (let [offset (- (count s) (count (str/triml s))) ;; index offset due to removal of whitespace
        s (str/trimr s)
        [asts sources source-order]
        (loop [i offset asts {} sources {} source-order []]
          (if (> i (dec (count s)))
            [asts sources source-order]
            (let [ast (parse-or-throw token-rule-parser s i :start :S :partial true)
                  [_ j] (insta/span ast)
                  t (xform.brics/transform ast)]
              (recur (+ i j)
                     (conj asts t)
                     (if (seq t)
                       (conj sources {(first (keys t)) (subs s i (+ i j))})
                       sources)
                     (if (seq t)
                       (conj source-order (first (keys t)))
                       source-order)))))]
    {:regexes (xform.common/substitute-all asts)
     :sources sources
     :source-order source-order}))

(defn ast->brics
  "Produce brics-automaton compatible input string from an AST.  Is incompatible with ASTS with named references."
  [x]
  (if (= :production (first x))
    (let [[k v] (first (xform.brics/transform x))]
      (str (name k) " = " (apply str v) " ;"))
    (apply str (xform.brics/transform x))))

(defn source->brics
  "Produce brics-automaton compatible input string from a Parsimony regular expression input string"
  [s]
  (ast->brics (regexp-parser s)))
