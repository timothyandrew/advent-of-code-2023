(ns adv.1
  (:require [clojure.java.io :as io]
    [clojure.string :as str]))

(def VALID_NUMS {"1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9
                  "one" 1 "two" 2 "three" 3 "four" 4 "five" 5
                  "six" 6 "seven" 7 "eight" 8 "nine" 9})

(defn to-num [n]
  (try
    (Integer/parseInt (str n))
    (catch Exception e nil)))

(defn detect-numbers-a [line]
  (+
    (* 10 (some to-num line))
    (some to-num (reverse line))))

(defn detect-numbers-b
  "Naive strategy, use a prefix tree for better lookup"
  [line]
  (for [i (range 0 (count line))]
    (let [line (drop i line)
           prefixes (rest (for [j (range 0 (inc (count line)))]
                            (subs (apply str line) 0 j)))]
      (some #(VALID_NUMS %) prefixes))))

(defn solve-a []
  (let [input (slurp (io/resource "input/1.txt"))
        lines (str/split input #"\n")]
    (apply +
      (map detect-numbers-a lines))))

(defn solve-b []
  (let [input (slurp (io/resource "input/1.txt"))
        lines (str/split input #"\n")]
    (apply +
      (map (fn [line]
             (let [numbers (detect-numbers line)]
               (+
                 (* 10 (some identity numbers))
                 (some identity (reverse numbers)))))
        lines))))
