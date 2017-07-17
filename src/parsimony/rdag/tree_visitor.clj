(ns parsimony.rdag.tree-visitor
  (:require [clojure.zip :as zip]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- -visit [loc state visitors]
  (reduce
   (fn [{:keys [node state skip] :as ctx} v]
     (if skip ctx (merge ctx (v node state))))
   {:node (zip/node loc) :state state :skip false}
   visitors))

(defn- rightmost-leaf
  "Return zipper to the righmost leaf node of the tree at loc"
  [loc]
  (loop [loc loc]
    (if-let [left-child-loc (zip/down loc)]
      (recur (zip/rightmost left-child-loc)) ;; go down one level
      loc)))  ;; there are no more levels, we're at the rightmost leaf

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn preorder-visit
  [ast state visitors]
  (loop [loc (zip/vector-zip ast) state state]
    (let [{:keys [node state skip]} (-visit loc state visitors)
          loc (if (identical? node (zip/node loc)) loc (zip/replace loc node))]
      (if (zip/end? (zip/next loc))
        {:node (zip/root loc) :state state}
        (recur (zip/next loc) state)))))

(defn reverse-preorder-visit
  [ast state visitors]
  (loop [loc (rightmost-leaf (zip/vector-zip ast)) state state]
    (let [{:keys [node state skip]} (-visit loc state visitors)
          loc (if (identical? node (zip/node loc)) loc (zip/replace loc node))]
      (if-not (zip/up loc)
        {:node (zip/root loc) :state state}
        (recur (zip/prev loc) state)))))

(defn preorder-demo
  []
  (preorder-visit [:a [:b [:c :d] [:e [:f :g [:h :i :j]]]]]
                  nil
                  [(fn [node state] (println node))]))

(defn reverse-preorder-demo
  []
  (reverse-preorder-visit [:a [:b [:c :d] [:e [:f :g [:h :i :j]]]]]
                          nil
                          [(fn [node state] (println node))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matchers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn skip-leaves []
  (fn [node state]
    (when-not (vector? node)
      {:skip true})))

(defn on [kws]
  (fn [node state]
    (when-not (contains? kws (first node))
      #_(println "on" kws node)
      {:skip true})))

(defn collect []
  (fn [node state]
    {:state (conj state node)}))

(defn >n-children [n]
  (fn [node state]
    (when-not (> (dec (count node)) n)
      #_(println ">n-children" node)
      {:skip true})))

(defn -or [& fs]
  (fn [node state]
    ;; if every visitor says to skip, then that means none matched
    (when (every? #(:skip (% node state)) fs)
      #_(println "-or" node)
      {:skip true})))

(defn -and [& fs]
  (fn [node state]
    ;; if some visitor says to skip, then that means not all have matched
    (when (some #(:skip (% node state)) fs)
      #_(println "-and" node)
      {:skip true})))
