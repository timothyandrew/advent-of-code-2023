(ns adv.7
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def card-strength {\A 14 \K 13 \Q 12 \J 11
                    \T 10 \9 9 \8 8 \7 7 \6 6
                    \5 5 \4 4 \3 3 \2 2})

(defn compare-hands [h1 h2]
  (let [h1 (:hand h1)
        h2 (:hand h2)
        hs1 (hand-strength h1)
        hs2 (hand-strength h2)]
    (cond
      (> hs1 hs2) 1
      (< hs1 hs2) -1
      :else (let [values
                  (for [[c1 c2] (partition 2 (interleave h1 h2))
                        :let [s1 (card-strength c1)
                              s2 (card-strength c2)]]
                    (cond
                      (> s1 s2) 1
                      (< s1 s2) -1
                      :else 0))]
              (first (filter #(not (zero? %)) values))))))

(defn hand-strength [hand]
  (let [counts (-> hand frequencies vals sort reverse)]
    (case counts
      [5] 700
      [4 1] 600
      [3 2] 500
      [3 1 1] 400
      [2 2 1] 300
      [2 1 1 1] 200
      [1 1 1 1 1] 100)))

(defn parse-hand [s]
  (let [[hand bid-str] (str/split s #" ")]
    {:hand hand :bid (Integer/parseInt bid-str)}))

(defn solve-a []
  (let [input (slurp (io/resource "input/7.txt"))
        input (str/split input #"\n")
        hands (map parse-hand input)
         hands (sort compare-hands hands)]
    (reduce +
      (map-indexed
        (fn [i hand]
          (* (:bid hand) (inc i)))
        hands))))
