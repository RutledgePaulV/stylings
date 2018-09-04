(ns stylings.extract
  (:require [hickory.core :as hickory]
            [clojure.java.io :as io])
  (:import (org.openqa.selenium.chrome ChromeDriver ChromeOptions)
           (org.openqa.selenium.support.ui WebDriverWait ExpectedConditions)
           (org.openqa.selenium.remote RemoteWebDriver)))


(def ^String CHROME "/Applications/Google Chrome Canary.app/Contents/MacOS/Google Chrome Canary")

(defn get-driver []
  (ChromeDriver.
    (doto (ChromeOptions.)
      (.setBinary CHROME)
      (.setHeadless true))))

(defn resolve-page [driver url]
  (.get driver url))

(defn install-ajax-hook [driver]
  (let [waiter (slurp (io/resource "waiter.js"))]
    (.executeScript driver waiter (into-array Object []))))

(defn wait-until-ajax-complete [driver timeout]
  (let [script "return window.__xhrCount===0;"]
    (doto (WebDriverWait. driver timeout)
      (.until (ExpectedConditions/jsReturnsValue script)))))

(defn get-current-markup [driver]
  (let [script "return new XMLSerializer().serializeToString(document);"]
    (.executeScript driver script (into-array Object []))))

(defn parse-html [html]
  (->> (hickory/parse html) (hickory/as-hickory)))

(defn load-page [^RemoteWebDriver driver url]
  (resolve-page driver url)
  (install-ajax-hook driver)
  (wait-until-ajax-complete driver 10))

(defn extract-html [^RemoteWebDriver driver]
  (->> (get-current-markup driver) (parse-html)))

(defn fetch [^RemoteWebDriver driver url]
  (load-page driver url)
  (extract-html driver))
