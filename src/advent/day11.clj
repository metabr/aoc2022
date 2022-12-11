(ns advent.day11
  (:require [clojure.string :as string]))


(def monkey-business-init (zipmap (range 8) (repeat 0)))
(def monkey-business (atom monkey-business-init))


(defn build-op [s]
  (let [[x op y] (-> s
                     (string/replace #"old" "%")
                     (string/split #" "))
        lambda (str "#(" op " " x " " y ")")]
    (-> lambda read-string eval)))


(defn parse-items [items]
  (->> (string/split items #", ")
       (mapv read-string)))


(defn parse-monkey [s]
  (let [monkey-matcher
        (re-matcher #"(?m)Monkey (\d)" s)
        items-matcher
        (re-matcher #"(?m)Starting items: ([\d, ]+)$" s)
        op-matcher
        (re-matcher #"(?m)Operation: new = (.+)$" s)
        test-matcher
        (re-matcher #"(?m)Test: divisible by (\d+)" s)
        true-monkey-matcher
        (re-matcher #"(?m)If true: throw to monkey (\d)" s)
        false-monkey-matcher
        (re-matcher #"(?m)If false: throw to monkey (\d)" s)
        id (-> monkey-matcher re-find last read-string)]
    [id
     {:id           id
      :items        (-> items-matcher re-find last parse-items)
      :op           (-> op-matcher re-find last build-op)
      :divisible-by (-> test-matcher re-find last read-string)
      :true-monkey  (-> true-monkey-matcher re-find last read-string)
      :false-monkey (-> false-monkey-matcher re-find last read-string)}]))


(defn inspect! [monkey item]
  (let [worry-level ((:op monkey) item)
        worry-level (int (/ worry-level 3))]
    (swap! monkey-business update (monkey :id) inc)
    (if (zero? (rem worry-level (:divisible-by monkey)))
      [worry-level (:true-monkey monkey)]
      [worry-level (:false-monkey monkey)])))


(defn inspect2! [lcm monkey item]
  (let [worry-level ((:op monkey) item)
        worry-level (mod worry-level lcm)]
    (swap! monkey-business update (monkey :id) inc)
    (if (zero? (rem worry-level (:divisible-by monkey)))
      [worry-level (:true-monkey monkey)]
      [worry-level (:false-monkey monkey)])))


(defn round! [monkeys inspect-fn]
  (reduce
   (fn [monkeys i]
     (let [m (get monkeys i)
           items (get m :items)]
       (assoc-in
        (->> (map (partial inspect-fn m) items)
             (reduce
              (fn [monkeys [level monkey-index]]
                (update-in monkeys [monkey-index :items] #(conj % level)))
              monkeys))
        [i :items]
        [])))
   monkeys
   (range (count monkeys))))


(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))


(defn lcm [a b]
  (/ (* a b) (gcd a b)))


(defn run [& _]
  (let [monkeys (->> (string/split (slurp "inputs/day11") #"\n\n")
                     (mapcat parse-monkey)
                     (apply hash-map))
        lcm (reduce lcm (map #(-> % val :divisible-by) monkeys))]
    (reduce (fn [monkeys _] (round! monkeys inspect!)) monkeys (range 20))
    (println "Monkey business level:"
             (->>
              (sort-by val > @monkey-business)
              (take 2) vals (reduce *)))
    (reset! monkey-business monkey-business-init)
    (reduce (fn [monkeys _] (round! monkeys (partial inspect2! lcm))) monkeys (range 10000))
    (println "Monkey business level:"
             (->>
              (sort-by val > @monkey-business)
              (take 2) vals (reduce *)))))
