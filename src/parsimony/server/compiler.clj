(ns parsimony.server.compiler
  "Component that interfaces with underlying token-definition compiler"
  (:import [clojure.lang ExceptionInfo])
  (:require [com.stuartsierra.component :as component]
            [parsimony.rdag.compiler :as compiler]
            [parsimony.log :as log]
            [schema.core :as s]))

(defrecord CompilerComponent [config]
  component/Lifecycle
  (start [this]
    (log/debug "CompilerComponent::start")
    this)
  (stop [this]
    (log/debug "CompilerComponent::stop")
    this))

(defn new-compiler [config]
  (map->CompilerComponent {:config (select-keys config [])}))

(defn- format-expecting
  "An Instaparse failure reason is a vector of maps, each of which has a :tag and :expecting field.
  For those tagged :regexp, the corresponding :expecting field is a Pattern.  We munge the Pattern
  back into a String for proper transmission to the client. "
  [m]
  (case (:tag m)
    :regexp (update m :expecting str)
    m))

(defn- format-instaparse-failure
  "Convert an instaparse Failure object into a standard hash-map"
  [failure]
  (-> (into {} failure)
      (update :reason
              #(into []
                     (map format-expecting)
                     %))))

(defn- format-exception
  "Given a compiler exception, convert to format acceptable for client-side consumption"
  [e]
  (let [data (ex-data e)]
    (if-let [causes (:causes data)]
      (let [failure
            (cond
              (:bad-regex causes) (select-keys data [:causes :source-str])
              (:parse-failure causes)
              (-> data
                  (select-keys [:causes :index :failure])
                  (update :failure format-instaparse-failure))
              :else
              (throw (ex-info "Unable to format exception:"
                              {:causes #{:unknown-cause} :exception e})))]
        {:failure failure})
      (throw (ex-info "Unable to format exception:"
                      {:causes #{:missing-causes-field} :exception e})))))

(defn- format-success
  "Given a compiled lexer in compiler/vec-schema, format it for client-side consumption"
  [lexer]
  {:success
   (into []
         (map #(update % 1 dissoc :automaton))
         lexer)})

(def ws-lex-info
  (-> "ws = [ \\n\\r\\t]+ ;"
      (compiler/make-str->vec)
      (first)))

(defn- maybe-inject-ws
  "If the given lexer does not contain a :ws token, then add it at the beginning"
  [lexer]
  (if (some #(= :ws (first %)) lexer)
    lexer
    (into [ws-lex-info] lexer)))

(def comment-lex-info
  (-> "comment = /\\* (/ | \\*+? !(/ | \\*))* \\*+ / ;"
      (compiler/make-str->vec)
      (first)))

(defn- maybe-inject-comment
  "If the given lexer does not contain a :comment token, then add it at the beginning"
  [lexer]
  (if (some #(= :comment (first %)) lexer)
    lexer
    (into [comment-lex-info] lexer)))

(def line-comment-lex-info
  (-> "line-comment = \\/\\/ ![\\n]* ;"
      (compiler/make-str->vec)
      (first)))

(defn- maybe-inject-line-comment
  "If the given lexer does not contain a :line-comment token, then add it at the beginning"
  [lexer]
  (if (some #(= :line-comment (first %)) lexer)
    lexer
    (into [line-comment-lex-info] lexer)))

(defn compile-lexer
  "Given a token-definition formatted string, return compiled lexer in compiler/vec-schema format"
  [this s]
  (try
    (-> s
        (compiler/make-str->vec)
        (maybe-inject-ws)
        (maybe-inject-comment)
        (maybe-inject-line-comment)
        (format-success))
    (catch ExceptionInfo e
      (format-exception e))))
