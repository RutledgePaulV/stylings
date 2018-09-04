(ns stylings.structural
  (:require [clojure.core.match :as match]
            [clojure.walk :as walk]))

(def tag-whitelist
  #{:div :img :nav :ul :li :a :header :footer :table :body :html :head :p :h1 :h2 :h3 :h4 :h5 :h6})

(defn filter-structural-tags [form]
  (match/match [form]
    [{:type :element :tag (a :guard tag-whitelist)}] form
    [{:type :element :tag _}] nil
    [{:type :comment}] nil
    :else form))

(defn remove-nil-content [form]
  (match/match [form]
    [{:type :element}] (update form :content (partial into [] (filter (every-pred some? (complement string?)))))
    :else form))

(defn hickory->sequence [hickory]
  (tree-seq :content :content hickory))

(defn find-root [hickory]
  (->> hickory
       (hickory->sequence)
       (drop-while
         (complement
           (fn [form]
             (match/match [form]
               [{:type :element :tag :html}] true
               :else false))))
       (first)))

(defn transform [hick]
  (let [transforms [filter-structural-tags remove-nil-content]]
    (walk/postwalk (apply comp transforms) hick)))

(defn structural [hick]
  (walk/postwalk
    (fn [form]
      (match/match [form]
        [{:type :element}]
        (let [tag      (get form :tag)
              children (get form :content [])]
          (if (empty? children)
            [tag]
            [tag children]))
        :else form))
    hick))

(defn get-hrefs [hick]
  (->> hick
       (hickory->sequence)
       (keep
         (fn [node]
           (match/match [node]
             [{:type :element :tag :a :attrs {:href (a :guard some?)}}] a
             :else nil)))
       (distinct)))

(defn get-structure
  "Returns just the tag structure of the page."
  [hick]
  (->> hick
       (find-root)
       (transform)
       (structural)))
