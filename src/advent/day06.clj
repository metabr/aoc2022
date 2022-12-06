(ns advent.day06)


(defn unique-or-repetition-offset [s]
  (loop [s s offset 1]
    (if (empty? s)
      true
      (if (some (set (take 1 s)) (rest s))
        offset
        (recur (rest s) (inc offset))))))


(defn start-of [s len]
  (loop [i 0 s s]
    (let [r (unique-or-repetition-offset (take len s))]
      (if (true? r)
        (+ i len)
        (recur (+ i r) (drop r s))))))


(defn run [& _]
  (let [input (slurp "inputs/day06")
        start-of-packet (start-of input 4)
        start-of-message (start-of input 14)]
    (println start-of-packet)
    (println start-of-message)))
