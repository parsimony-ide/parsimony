(ns parsimony.rdag.compiler
  (:require [alanlcode.util.os :as os]
            [parsimony.rdag.automaton :as automaton]
            [parsimony.rdag.syntax :as syntax]
            [schema.core :as s]))

(s/defschema vec-schema
  [[(s/one s/Keyword "token-name")
    (s/one  {:automaton automaton/automaton-klass
             :js-automaton automaton/js-automaton-schema
             :regex-str s/Str
             :source-str s/Str
             }
            "token-definition")]])

(s/defschema map-schema
  {s/Keyword {:automaton automaton/automaton-klass
              :regex-str s/Str
              :source-str s/Str
              }})

;; The reason for having two schemas: in some cases, we just want an output map
;; for easy lookup and access.  In other cases, we need to preserve the
;; original file ordering since this determines order of preference for
;; tokenization

(defn make-str->vec [s]
  {:post [(s/validate vec-schema %)]}
  (let [{:keys [regexes sources source-order]} (syntax/definition-parser s)]
    (into []
          (map (fn [k]
                 (let [regex-str (get regexes k)
                       source-str (get sources k)]
                   (try
                     (let [a (automaton/str->automaton regex-str)]
                       [k
                        (sorted-map
                         :automaton a
                         ;; :fast-automaton (automaton/optimize a)
                         :js-automaton (automaton/automaton->js-automaton a)
                         :regex-str regex-str
                         :source-str source-str)])
                     (catch IllegalArgumentException e
                       (throw (ex-info (str "Unable to compile automaton for regex named " k)
                                       {:causes #{:bad-regex}
                                        :exception e :regex-str regex-str :source-str source-str})))))))
          source-order)))

(defn make-str->map [s]
  {:post [(s/validate map-schema %)]}
  (into {}
        (map #(update % 1 select-keys [:automaton :regex-str :source-str]))
        (make-str->vec s)))

(defn make-file->vec [tokens-file]
  (make-str->vec (slurp (os/->file tokens-file))))

(defn make-file->map [tokens-file]
  (make-str->map (slurp (os/->file tokens-file))))
