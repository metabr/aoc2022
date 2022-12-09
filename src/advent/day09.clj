(ns advent.day09
  (:require [clojure.string :as string]))


(def visited (atom #{[0 0]}))
(def H (atom [0 0]))
(def T (atom [0 0]))

(def visited2 (atom #{[0 0]}))
(def H2 (atom [0 0]))
(def K1 (atom [0 0]))
(def K2 (atom [0 0]))
(def K3 (atom [0 0]))
(def K4 (atom [0 0]))
(def K5 (atom [0 0]))
(def K6 (atom [0 0]))
(def K7 (atom [0 0]))
(def K8 (atom [0 0]))
(def K9 (atom [0 0]))


(defn left [[x y]] [(dec x) y])
(defn right [[x y]] [(inc x) y])
(defn up [[x y]] [x (inc y)])
(defn down [[x y]] [x (dec y)])


(defn move-knot-after [knot-T knot-H]
  (let [[hx hy] knot-H [tx ty] knot-T
        move-fn
        (cond
          (= knot-H knot-T) ;; same place
          identity
          ;; touching
          (and (>= 1 (abs (- hx tx)))
               (>= 1 (abs (- hy ty))))
          identity
          ;; same column
          (= hx tx) (if (> hy ty) up down)
          ;; same row
          (= hy ty) (if (> hx tx) right left)
          ;; diagonally
          (and (> hx tx) (> hy ty))
          (comp right up)
          (and (> hx tx) (< hy ty))
          (comp right down)
          (and (< hx tx) (> hy ty))
          (comp left up)
          (and (< hx tx) (< hy ty))
          (comp left down))]
    (move-fn knot-T)))


(defn move! [[direction steps]]
  (let [head-move-fn (case direction "R" right "L" left "U" up "D" down)]
    (dotimes [_ (read-string steps)]
      (swap! H head-move-fn)
      (swap! T move-knot-after @H)
      (swap! visited conj @T))))


(defn move2! [[direction steps]]
  (let [head-move-fn (case direction "R" right "L" left "U" up "D" down)]
    (dotimes [_ (read-string steps)]
      (swap! H2 head-move-fn)
      (swap! K1 move-knot-after @H2)
      (swap! K2 move-knot-after @K1)
      (swap! K3 move-knot-after @K2)
      (swap! K4 move-knot-after @K3)
      (swap! K5 move-knot-after @K4)
      (swap! K6 move-knot-after @K5)
      (swap! K7 move-knot-after @K6)
      (swap! K8 move-knot-after @K7)
      (swap! K9 move-knot-after @K8)
      (swap! visited2 conj @K9))))


(defn run [& _]
  (let [input (->> (slurp "inputs/day09")
                   string/split-lines
                   (map #(string/split % #" ")))]
    (doall (map move! input))
    (println "Part 1: Tail visited" (count @visited) "positions")
    (doall (map move2! input))
    (println "Part 2: Tail visited" (count @visited2) "positions")))
