(ns day11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; part 1

(defn read-input []
  (let [in (-> (slurp (io/resource "input11.txt"))
               (str/split #"\n"))
        in (map (comp vec seq)in)]
    (vec in)))

(def in (read-input))
(def n (count in))
(def m (count (first in)))

(defn get-mat [mat i j]
  (nth (nth mat i) j))

(defn set-mat! [mat i j e]
  (assoc mat i (assoc (nth mat i) j e)))

(defn occupied [old i j]
  (->> (for [i (range (max 0 (dec i)) (min n (+ i 2)))
             j (range (max 0 (dec j)) (min m (+ j 2)))]
         [i j])
       (remove #(= % [i j]))
       (reduce (fn [res [i j]] (if (= (get-mat old i j) \#) (inc res) res)) 0)))

(defn one-iteration [occupied threshold mat]
  (->> (for [i (range n) j (range m)]
         [i j])
       (reduce (fn [new [i j]]
                 (let [e (get-mat mat i j)]
                   (case e
                     \. new
                     \# (if (>= (occupied mat i j) threshold)
                          (set-mat! new i j \L)
                          new)
                     \L (if (= (occupied mat i j) 0)
                          (set-mat! new i j \#)
                          new))))
               mat)))

;; fixed point
(->> (iterate (partial one-iteration occupied 4) in)
     (partition 2 1)
     (drop-while #(apply not= %))
     first
     first
     flatten
     (filter #(= % \#))
     count)

;; part 2

(defn check-bounds [i j]
  (and (<= 0 i) (<= 0 j)
       (< i n) (< j m)))

(defn see [mat x y i j]
  (loop [cur-x (+ x i) cur-y (+ y j)]
    (if (check-bounds cur-x cur-y)
      (case (get-mat mat cur-x cur-y)
        \. (recur (+ cur-x i) (+ cur-y j))
        \# 1
        \L 0)
      0)))

(defn occupied2 [old x y]
  (->> (for [i (range -1 2)
             j (range -1 2)]
         [i j])
       (remove #(= % [0 0]))
       (reduce (fn [res [i j]] (+ res (see old x y i j))) 0)))

(->> (iterate (partial one-iteration occupied2 5) in)
     (partition 2 1)
     (drop-while #(apply not= %))
     first
     first
     flatten
     (filter #(= % \#))
     count)
