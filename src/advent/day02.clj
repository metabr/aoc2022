(ns advent.day02
  (:require [clojure.string :as string]))


(def shape {"X" "Rock" "Y" "Paper" "Z" "Scissors"
             "A" "Rock" "B" "Paper" "C" "Scissors"})
(def beats {"Rock" "Scissors"
            "Paper" "Rock"
            "Scissors" "Paper"})
(def loses (zipmap (vals beats) (keys beats)))

(defn outcome [opponent me]
  (cond
    ;; draw
    (= (shape opponent) (shape me)) 3
    ;; win
    (= (beats (shape me)) (shape opponent)) 6
    ;; loss
    :else 0))


(defn shape-score [s]
  (let [s (if (> (count s) 1) s (shape s))]
    (case s "Rock" 1 "Paper" 2 "Scissors" 3)))


(defn round-score [[opponent me]]
  (+ (outcome opponent me)
     (shape-score me)))


(defn round-score-2 [[opponent desired-outcome]]
  (case desired-outcome
    ;; lose
    "X" (shape-score (beats (shape opponent)))
    ;; draw
    "Y" (+ 3 (shape-score (shape opponent)))
    ;; win
    "Z" (+ 6 (shape-score (loses (shape opponent))))))


(defn run [& _]
  (let [input (->> (string/split (slurp "inputs/day02") #"\n")
                   (map #(string/split % #" ")))]
    (println "Part 1 total score:" (reduce + (map round-score input)))
    (println "Part 2 total score:" (reduce + (map round-score-2 input)))))
