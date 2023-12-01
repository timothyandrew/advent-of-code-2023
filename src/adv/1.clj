(ns adv.1
  (:require [clojure.java.io :as io]
    [clojure.string :as str]))

(defn to-num [n]
  (try
    (Integer/parseInt (str n))
    (catch Exception e nil)))

(defn count-nums [line]
  (+
    (* 10 (some to-num line))
    (some to-num (reverse line))))

(defn solve-a []
  (let [input (slurp (io/resource "input/1a.txt"))
        lines (str/split input #"\n")]
    (apply + (map count-nums lines))))
