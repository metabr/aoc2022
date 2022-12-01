(ns advent.day01
  (:require [clojure.string :as string]))


(defn run [& _]
  (let [elf-calories
        (fn [elf] (->> (string/split elf #"\n")
                       (map read-string)
                       (reduce +)))
        elves (->> (string/split (slurp "inputs/day01") #"\n\n")
                   (map elf-calories))]
    (println "Elf carrying most calories carries" (apply max elves) "of them.")
    (println "Top three elves carrying"
             (->> (sort > elves)
                  (take 3)
                  (reduce +))
             "calories in total.")))
