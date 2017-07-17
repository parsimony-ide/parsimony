(ns parsimony.common
  (:require [clojure.java.io :as io]
            [taoensso.nippy :as nippy]))

(defn vec-join [sep args]
  (vec (interpose sep args)))

(defn serialize [x f]
  (with-open [os (io/output-stream f)
              dos (java.io.DataOutputStream. os)]
    (nippy/freeze-to-out! dos x)))

(defn deserialize [f]
  (with-open [is (io/input-stream f)
              dis (java.io.DataInputStream. is)]
    (nippy/thaw-from-in! dis)))
