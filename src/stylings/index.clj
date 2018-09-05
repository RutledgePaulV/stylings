(ns stylings.index
  (:require [clojure.set :as sets])
  (:import (smile.neighbor BKTree Neighbor)
           (smile.math.distance Metric)
           (java.util ArrayList)))


(defn jaccard-distance []
  (proxy [Metric] []
    (d [t1 t2]
      (let [inter (count (sets/intersection t1 t2))
            union (count (sets/union t1 t2))]
        (Math/round ^Double (* 100 (- 1.0 (/ inter union))))))))


(defn subtrees [tree]
  (if (or (nil? tree) (empty? tree))
    []
    (let [parent (first tree) children (take-while vector? (rest tree))]
      (if (not-empty children)
        (concat
          (map (partial vector parent) children)
          (mapcat subtrees children)
          (subtrees (drop (inc (count children)) tree)))
        (subtrees (next tree))))))


(defprotocol Index
  (insert [this x])
  (search [this match max-distance]))


(defn create-index []
  (let [tree (doto (BKTree. (jaccard-distance))
               (.setIdenticalExcluded true))]

    (reify Index

      (insert [this x]
        (.add tree (set (subtrees x))))

      (search [this match max-percent-disjoint]
        (let [output (ArrayList.)]
          (.range tree (set (subtrees match)) (int max-percent-disjoint) output)
          (->> output
               (sort-by (fn [^Neighbor n] (.distance n)))
               (map (fn [^Neighbor n] (.value n)))))))))



