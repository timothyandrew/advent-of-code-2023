(ns adv.3
  (:require [clojure.string :as str]
    [clojure.java.io :as io]))

(defn is-num? [n]
  (try
    (Integer/parseInt (str n))
    true
    (catch Exception e false)))

(defn safe-get [grid [x y]]
  (if (and
        (< x (count grid))
        (< y (count (get grid x))))
    (get-in grid [x y])
    nil))

(defn neighbors [grid [x y]]
  (remove #(or
             (contains? #{nil \.} %)
             (is-num? %))
    [(safe-get grid [(inc x) y])
      (safe-get grid [(dec x) y])
      (safe-get grid [x (inc y)])
      (safe-get grid [x (dec y)])
      (safe-get grid [(inc x) (inc y)])
      (safe-get grid [(inc x) (dec y)])
      (safe-get grid [(dec x) (inc y)])
      (safe-get grid [(dec x) (dec y)])]))

(defn build-map [lines]
  (let [current (atom {:s "" :adj #{}})
         results (atom [])]
    (doseq [i (range (count lines))
           j (range (count (get lines i)))
           :let [n (get-in lines [i j])]]
      (when (is-num? n)
        (swap! current
          (fn [c] (-> c
                    (update :s str (str n))
                    (update :adj #(apply conj % (neighbors lines [i j])))))))
      (when (or
              (= j (dec (count (get lines i))))
              (not (is-num? n)))
        (swap!
          current
          (fn [acc]
            (when (not (empty? (:s acc)))
              (swap! results conj acc))
            {:s "" :adj #{}}))))
    @results))

(defn solve-a []
  (let [input (slurp (io/resource "input/3.txt"))
         lines (str/split input #"\n")
         grid (build-map lines)
         part-numbers (filter #(not (empty? (:adj %))) grid)]
    (apply +
      (map (fn [p] (-> p :s Integer/parseInt)) part-numbers))))
 
