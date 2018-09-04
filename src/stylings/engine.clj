(ns stylings.engine
  (:require [clojure.core.async :as async]
            [stylings.extract :as extract]
            [stylings.structural :as struct]
            [clojure.set :as sets]
            [clojure.string :as string]))


(defn get-html [url]
  (with-open [driver (extract/get-driver)]
    (extract/fetch driver url)))

(defn resolve-relative [base href]
  (cond
    (string/starts-with? href "https://")
    href

    (string/starts-with? href "http://")
    href

    (string/starts-with? href "/")
    (let [first-slash (string/index-of base "/" (count "https://"))]
      (str (if first-slash (subs base 0 first-slash) base) href))

    :otherwise (str base href)))

(defn has-extension? [s]
  (when-some [last-slash (string/last-index-of s "/" (count "https://"))]
    (some? (string/index-of s "." last-slash))))

(defn page? [url]
  (or (string/ends-with? url ".html")
      (string/ends-with? url "/")
      (not (has-extension? url))))

(defn extract-links [url structure]
  (->> (struct/get-hrefs structure)
       (map (partial resolve-relative url))
       (filter page?)))

(defn filter-structure [structure]
  (struct/get-structure structure))

(defn start [seeds parallelism output]
  (let [input (async/chan parallelism (distinct))]
    (letfn [(process-page [url]
              (println "now processing: " url)
              (try
                (let [structure (get-html url)
                      links     (extract-links url structure)]
                  (when (not-empty links)
                    (async/onto-chan input links false))
                  (filter-structure structure))
                (catch Exception e
                  {:error (.getMessage e)})))]
      (let [xf (map (juxt identity (memoize process-page)))]
        (async/pipeline-blocking parallelism output xf input))
      (async/onto-chan input seeds false)
      output)))

(defn collect
  "Returns a map of url -> tag structure by crawling the web
  beginning with the provided seed urls and expanding links within
  each scraped page."
  [seeds max-size]
  (let [result-ch (async/chan)
        aggregate (atom {})]
    (start seeds 4 result-ch)
    (async/<!!
      (async/go-loop []
        (when-some [[k v] (async/<! result-ch)]
          (let [results (swap! aggregate assoc k v)]
            (if (> (count results) max-size)
              (do (async/close! result-ch) @aggregate)
              (recur))))))))


(defn search [db s]
  (letfn [(subtrees [x]
            )
          (jaccard [x y]
            (/ (count (sets/intersection x y))
               (count (sets/union x y))))]
    (let [s* (subtrees s)]
      (->> (seq db)
           (map (fn [[k v]]
                  (let [v* (subtrees v)]
                    {:url       k
                     :structure v
                     :score     (jaccard v* s*)})))
           (sort-by :score)))))