(ns adv.5
  (:require [clojure.string :as str]
    [clojure.java.io :as io]))

(defn parse-match [match]
  (let [entries (-> match str/trim (str/split #"\n"))]
    (mapv (fn [e] (mapv #(Long/parseLong %) (str/split e #" "))) entries)))

(defn build-mapping [matches]
  (letfn [(perform-lookup [id mapping]
            (if-let [[dest source-start _]
                      (first
                        (filter (fn [[dest source-start n]]
                                  (and
                                    (>= id source-start)
                                    (<= id (+ source-start n))))
                          mapping))]
              (+ dest (- id source-start))
              id))]
    (fn [seed]
      (reduce perform-lookup seed (drop 1 matches)))))

(defn parse [input]
  (let [matches (re-matches #"(?sm)seeds: ((?:\d+\s?)*)\nseed-to-soil map:\n((?:\d+\s?)*)\nsoil-to-fertilizer map:\n((?:\d+\s?)*)\nfertilizer-to-water map:\n((?:\d+\s?)*)\nwater-to-light map:\n((?:\d+\s?)*)\nlight-to-temperature map:\n((?:\d+\s?)*)\ntemperature-to-humidity map:\n((?:\d+\s?)*)\nhumidity-to-location map:\n((?:\d+\s?)*).*" input)
         matches (drop 1 matches)
         matches (map parse-match matches)]
    {:lookup (build-mapping matches) :matches matches}))

(defn solve-a []
  (let [input (slurp (io/resource "input/5.txt"))
         {lookup :lookup [seeds _] :matches} (parse input)
         locations (map lookup (flatten seeds))]
    (apply min locations)))



