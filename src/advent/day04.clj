(ns advent.day04
  (:require [clojure.set :as set]
            [clojure.string :as string]))


(defn fully-contains? [[r1 r2]]
    (or (set/superset? r1 r2)
        (set/superset? r2 r1)))


(defn overlaps? [[r1 r2]]
  (not (empty? (set/intersection r1 r2))))


(defn run [& _]
  (let [input (->> (string/split-lines (slurp "inputs/day04"))
                   (map #(let [[elf1 elf2] (string/split % #",")
                               [s1 e1] (string/split elf1 #"-")
                               [s2 e2] (string/split elf2 #"-")]
                           (mapv read-string [s1 e1 s2 e2])))
                   (map (fn [[s1 e1 s2 e2]]
                          (let [r1 (set (range s1 (inc e1)))
                                r2 (set (range s2 (inc e2)))]
                            [r1 r2]))))]
    (println "One range fully contains the other in"
             (count (filter fully-contains? input))
             "cases")
    (println "Ranges overlap in"
             (count (filter overlaps? input))
             "pairs")))
