(ns advent.day05
  (:require [clojure.string :as string]))


(defn parse-crates [input]
  (let [input (reverse (string/split-lines input))
        ks (->> (string/split (first input) #" +")
                (filter (complement empty?))
                (map read-string))
        crates-indices (range 1 (count (first input)) 4)
        parse-row (fn [row] (map #(get row %) crates-indices))
        stacks (->> (rest input)
                    (map parse-row))]
    (loop [crates {} ks ks stacks stacks]
      (if (empty? ks)
        (let [k (keys crates)
              v (map (fn [s] (vec (remove #(= % \space) s))) (vals crates))]
          (zipmap k v))
        (recur (assoc crates (first ks) (map first stacks))
               (rest ks)
               (map rest stacks))))))


(defn parse-instructions [input]
  (->> (string/split-lines input)
       (map #(mapv read-string (string/split % #" ")))
       (map #(hash-map :move (get % 1) :from (get % 3) :to (get % 5)))))


(defn execute-instruction [crates {:keys [move from to]}]
  (let [f (get crates from [])
        t (get crates to [])
        t' (into t (identity (take-last move f))) ;; use reverse in place of identity for part 1
        f' (vec (drop-last move f))]
    (assoc crates from f' to t')))


(defn rearrange [crates instructions]
  (if (empty? instructions)
    crates
    (recur (execute-instruction crates (first instructions))
           (rest instructions))))


(defn run [& _]
  (let [[crates instructions] (string/split (slurp "inputs/day05") #"\n\n")
        crates (parse-crates crates)
        instructions (parse-instructions instructions)]
    (println (->> (rearrange crates instructions)
                  (into (sorted-map))
                  vals
                  (map last)
                  (apply str)))))
