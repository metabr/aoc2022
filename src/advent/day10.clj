(ns advent.day10
  (:require [clojure.string :as string]))


(defn crt-draw [[x i]]
  (let [sprite [(dec x) x (inc x)]]
    (if (some #{i} sprite)
      "#" ".")))


(defn run [& _]
  (let [input (->> (slurp "inputs/day10")
                   string/split-lines)
        xs
        (loop [xs [1] commands input]
          (if (empty? commands)
            xs
            (let [[cmd arg] (string/split (first commands) #" ")]
              (if (= "noop" cmd)
                (recur (conj xs (last xs)) (rest commands))
                (recur (-> (conj xs (last xs))
                           (conj (+ (last xs) (read-string arg))))
                       (rest commands))))))]
    (println "Sum of six signal strengths:"
             (->> (map #(* % (get xs (dec %))) '(20 60 100 140 180 220))
                  (reduce +)))
    (doall
     (->> (range 40)
          cycle
          (interleave xs)
          (partition 2)
          (map crt-draw)
          (partition 40)
          (map (partial apply str))
          (map println)))))
