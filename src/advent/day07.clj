(ns advent.day07
  (:require [clojure.string :as string]
            [clojure.walk :as w]))


(def fs (atom {"/" {}}))
(def cwd (atom []))
(def directories (atom []))


(defn parse-output [output]
  (doall
   (map #(let [[sz name] (string/split % #" ")]
           (swap! fs assoc-in (conj @cwd name)
                  (if (= sz "dir")
                    (do (swap! directories conj (conj @cwd name)) {}) (read-string sz))))
        output)))


(defn parse-command [command]
  (if (= (first command) "ls")
    (parse-output (rest command))
    (let [[_ arg] (string/split (first command) #" ")]
      (if (= arg "..")
        (swap! cwd #(vec (drop-last %)))
        (swap! cwd conj arg)))))


(defn size [fs path]
  (->> (get-in fs path)
       (map (fn [[k v]] (if (int? v) v (size fs (conj path k)))))
       (reduce +)))


(defn free-space [fs]
  (- 70000000 (size fs ["/"])))


(defn delete-dir [fs path]
  (assoc-in fs path 0))


(defn run [& _]
  (let [input (->> (string/split (slurp "inputs/day07") #"\$\ ")
                   rest (map string/split-lines))]
    (doall (map parse-command input))
    (->> (map (partial size @fs) @directories)
         (filter #(<= % 100000))
         (reduce +)
         (println "Part 1:"))
    (->> (map #(vector (free-space (delete-dir @fs %)) (size @fs %)) @directories)
         (filter #(>= (first %) 30000000))
         (map second)
         sort first
         (println "Part 2:"))))
