(ns parsimony.common-test
  (:require [alanlcode.util.os :as os]
            [clojure.test :refer [is]]))

(defmacro is-not [arg & args]
  `(is (not ~arg) ~@args))

(defn temp-db-file []
  (-> (os/make-temp-file (os/temp-directory) "temp-rdag" ".db")
      (os/->file)
      (doto (.deleteOnExit))))
