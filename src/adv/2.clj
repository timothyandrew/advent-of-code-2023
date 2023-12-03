(ns adv.2
  (:require [clojure.java.io :as io]
    [clojure.string :as str]))

(defn parse-game [game]
  (let [colors (str/split game #",")
         colors (map #(vec (str/split (str/trim %) #" ")) colors)
         colors (map (fn [[n c]] [c (Integer/parseInt n)]) colors)
         colors (apply hash-map (flatten colors))]
    colors))

(defn parse-line [line]
  (let [[_ id turns] (re-matches #"^Game (\d+):(.*)$" line)
         id (Integer/parseInt id)
         turns (->> (str/split turns #";") (map str/trim))
         turns (map parse-game turns)]
    {:id id :turns turns}))

(defn solve-a []
  (let [input (slurp (io/resource "input/2.txt"))
         lines (str/split input #"\n")
         games (map parse-line lines)
         max {"red" 12 "green" 13 "blue" 14}
         impossible
         (set
           (filter identity
             (for [{turns :turns id :id} games
                    turn turns]
               (if (some (fn [[k v]] (> (get turn k 0) v)) max)
                 id))))]
    (apply +
      (map :id
        (remove #(contains? impossible (:id %)) games)))))
