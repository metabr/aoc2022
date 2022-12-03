(ns advent.day03
  (:require [clojure.set :as set]
            [clojure.string :as string]))


(defn common-items [rucksack]
  (let [compartments (split-at (/ (count rucksack) 2) rucksack)
        c1 (set (first compartments))
        c2 (set (second compartments))]
    (vec (set/intersection c1 c2))))


(defn priority [item]
  (let [lower-case (map char (range 97 (+ 97 26)))
        upper-case (map char (range 65 (+ 65 26)))
        ks (->> [lower-case upper-case]
                (apply concat)
                (map str))
        p-map (zipmap ks (range 1 53))]
    (p-map (str item))))


(defn group-badge [[r1 r2 r3]]
  (->> (map set [r1 r2 r3])
       (apply set/intersection)
       vec))


(defn run [& _]
  (let [input (string/split-lines (slurp "inputs/day03"))]
    (println "Part 1. Sum of priorities:"
             (->> (map common-items input)
                  (apply concat)
                  (map priority)
                  (reduce +)))
    (println "Part 2. Sum of group-badge item priorities:"
             (->> (partition 3 input)
                  (map group-badge)
                  (apply concat)
                  (map priority)
                  (reduce +)))))
