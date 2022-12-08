(ns advent.day08
  (:require [clojure.string :as string]))


(defn dimensions [grid]
  [(count grid)
   (count (first grid))])


(defn mark-edges-visible [grid]
  (let [[w h] (dimensions grid)]
    (reduce
     (fn [grid idx] (assoc-in grid idx true))
     grid
     (concat
      (map #(vector 0 % 1) (range w))
      (map #(vector (dec h) % 1) (range w))
      (map #(vector % 0 1) (range h))
      (map #(vector % (dec w) 1) (range h))))))


(defn check-row [row]
  (loop [row row
         tallest (get-in row [0 0])
         idx 1]
    (if (= idx (dec (count row)))
      row
      (let [current (get-in row [idx 0])]
        (if (>= tallest current)
          (recur row tallest (inc idx))
          (recur (assoc-in row [idx 1] true) (max current tallest) (inc idx)))))))


(defn transpose-grid [grid]
  (apply mapv vector grid))


(defn mark-visibility [grid]
  (->>
   (mapv check-row grid) ;; from the left
   (mapv #(into [] (reverse %))) ;; flip
   (mapv check-row) ;; from the right
   transpose-grid
   (mapv check-row) ;; from the top
   (mapv #(into [] (reverse %))) ;; flip
   (mapv check-row))) ;; from the bottom


(defn trees-left [grid x y]
  (->> (get grid x)
       (take y)
       reverse
       (map first)
       vec))


(defn trees-right [grid x y]
  (->> (get grid x)
       (drop (inc y))
       (map first)
       vec))


(defn trees-top [grid x y]
  (trees-left (transpose-grid grid) y x))


(defn trees-bottom [grid x y]
  (trees-right (transpose-grid grid) y x))


(defn visible [height xs]
  (loop [v 0 xs xs]
    (if (empty? xs)
      v
      (if (>= (first xs) height)
        (inc v)
        (recur (inc v) (rest xs))))))


(defn scenic-score [grid [x y]]
  (let [height (get-in grid [x y 0])]
    (->>
     ((juxt trees-left trees-right trees-top trees-bottom) grid x y)
     (map (partial visible height))
     (reduce *))))


(defn calculate-scenic-scores [grid]
  (let [[w h] (dimensions grid)
        tree-coordinates
        (mapcat
         (fn [x]
           (map (fn [y] [x y])
                (range 1 (dec h))))
         (range 1 (dec w)))]
    (map (partial scenic-score grid) tree-coordinates)))


(defn run [& _]
  (let [input (->> (string/split-lines (slurp "inputs/day08"))
                   (mapv (fn [s]
                           (mapv
                            #(vector (Integer/parseInt (str %)) false) ;; [tree-height visibility]
                            s))))
        grid (->> (mark-edges-visible input)
                  mark-visibility)]
    (println "There is"
             (count (mapcat (partial filter (partial (comp true? second))) grid))
             "visible trees")
     (println "Highest scenic score is"
              (apply max (calculate-scenic-scores grid)))))
