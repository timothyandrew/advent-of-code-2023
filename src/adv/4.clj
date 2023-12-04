(ns adv.4
  (:require [clojure.java.io :as io]
    [clojure.string :as str]))

(defn parse-num-seq [nseq]
  (remove nil?
    (map #(if (empty? (str/trim %))
            nil
            (Integer/parseInt %))
      (str/split (str/trim nseq) #" "))))

(defn parse-card [line]
  (let [[_ id wins mine] (re-matches #"^Card\s+(\d+):\s+((?:\d+\s*)*)\|\s+((?:\d+\s*)*)$" line)
         id (Integer/parseInt id)
         wins (parse-num-seq wins)
         mine (parse-num-seq mine)]
    {:id id :wins wins :mine mine}))

(defn count-matches [card]
  (let [wins (set (:wins card))
         matches (for [m (:mine card)
                       :when (contains? wins m)]
                   card)]
    {:count (count matches)
      :score (int (Math/pow 2 (dec (count matches))))}))

(defn count-copies [counts card]
  (let [matches (count-matches card)
         copies (take (:count matches) (iterate inc (inc (:id card))))]
    (reduce (fn [counts copy-id]
              (if (get counts copy-id)
                (update counts copy-id + (get counts (:id card)))
                counts))
      counts
      copies)))

(defn solve-a []
  (let [input (slurp (io/resource "input/4.txt"))
         lines (str/split input #"\n")
         cards (map parse-card lines)
         matches (map count-matches cards)]
    (apply + (map :score matches))))

(defn solve-b []
  (let [input (slurp (io/resource "input/4.txt"))
         lines (str/split input #"\n")
         cards (map parse-card lines)
         counts (apply hash-map (interleave
                                  (map :id cards)
                                  (repeat 1)))]
    (->> (reduce count-copies counts cards)
      vals
      (apply +))))
