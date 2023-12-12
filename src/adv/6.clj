(ns adv.6
  (:require [clojure.string :as str]
    [clojure.java.io :as io]))

(defn as-num [n]
  (try
    (Integer/parseInt (str n))
    (catch Exception e nil)))

(defn parse [lines]
  (let [[time distance]
         (for [line lines]
           (->> (str/split line #" ") (map as-num) (remove nil?)))]
    (->>
      (interleave time distance)
      (partition 2)
      (map (fn [[t d]] (hash-map :time t :distance d))))))

(defn calc-distances [race]
  (for [hold-time (range 1 (:time race))
         :let [runtime (- (:time race) hold-time)]]
    (* hold-time runtime)))

(defn solve-a []
  (let [input (slurp (io/resource "input/6.txt"))
         input (str/split input #"\n")
         races (parse input)]
    (->>
      (for [race races
             :let [distances (calc-distances race)]]
        (filter #(> % (:distance race)) distances))
      (map count)
      (apply *))))
