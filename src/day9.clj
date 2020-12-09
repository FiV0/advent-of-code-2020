(ns day9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; part 1

(defn read-input []
  (let [in (-> (slurp (io/resource "input9.txt"))
               (str/split #"\n"))
        in (map #(Long/parseLong %) in)]
    (vec in)))

(def in (read-input))

(defn remove-ith [s i]
  (concat (take i s) (drop (inc i) s)))

(defn possible? [s n]
  (reduce (fn [_ i] (when ((set (remove-ith s i)) (- n (nth s i))) (reduced true))) nil (range (count s))))

(possible? (range 25) 25)
(possible? (range 25) 48)

(defn solve [in]
  (loop [cur in]
    (if-not (possible? (take 25 cur) (first (drop 25 cur)))
      (first (drop 25 cur))
      (recur (rest cur)))))

(solve in)

;; part 2
(def res (solve in))

(defn solve2 [in res]
  (loop [cur1 in cur2 clojure.lang.PersistentQueue/EMPTY val 0]
    (cond (= val res)
          (+ (apply max cur2)
             (apply min cur2))
          (< val res)
          (recur (rest cur1) (conj cur2 (first cur1)) (+ val (first cur1)))
          :else
          (recur cur1 (pop cur2) (- val (peek cur2))))))

(solve2 in res)
