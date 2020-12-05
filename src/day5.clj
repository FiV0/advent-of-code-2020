(ns day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; part 1
(defn read-input []
  (let [in (-> (slurp (io/resource "input5.txt"))
               (clojure.string/split #"\n"))
        in (map #(vector (subs % 0 7) (subs % 7)) in)]
    in))

(def in (read-input))

(defn search [x y n s]
  (loop [i 0 j n s (seq s)]
    (let [mid (int (/ (+ i j) 2))]
      (cond (= i j) i
            (= (first s) x) (recur i mid (rest s))
            :else (recur (inc mid) j (rest s))))))

(->> (map #(+ (* (search \F \B 127 (first %)) 8) (search \L \R 7 (second %))) in)
     (apply max))

;; part 2

(def res (->> (map #(+ (* (search \F \B 127 (first %)) 8) (search \L \R 7 (second %))) in)
              sort))

(doseq [[x y] (map #(vector %1 %2) res (rest res))]
  (when (> (- y x) 1)
    (println (inc x))))

;; more clojure like
(->> (map #(vector %1 %2) res (rest res))
     (reduce (fn [res [x y]] (when (> (- y x) 1) (reduced (inc x)))) nil))
