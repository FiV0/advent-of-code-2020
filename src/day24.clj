(ns day24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

;; part 1

(defn parse-line [s]
  (cond (= (first s) \e) (cons [-2 0] (parse-line (rest s)))
        (= (first s) \w) (cons [2 0] (parse-line (rest s)))
        (= (take 2 s) '(\s \e)) (cons [-1 -1] (parse-line (drop 2 s)))
        (= (take 2 s) '(\s \w)) (cons [1 -1]  (parse-line (drop 2 s)))
        (= (take 2 s) '(\n \e)) (cons [-1 1] (parse-line (drop 2 s)))
        (= (take 2 s) '(\n \w)) (cons [1 1] (parse-line (drop 2 s)))))

(defn read-input []
  (let [in (-> (slurp (io/resource "input24.txt"))
               (str/split #"\n"))]
    (map parse-line in)))

(def in (read-input))

(defn add-vec [v1 v2]
  (map + v1 v2))

(->> (map #(reduce add-vec [0 0] %) in)
     frequencies
     (reduce-kv (fn [res _ v] (if (odd? v) (inc res) res)) 0 ))
;; => 382

;; part 2

(def start (->> (map #(reduce add-vec [0 0] %) in)
                frequencies
                (reduce-kv (fn [res k v] (if (odd? v) (conj res k) res)) #{})))

(def nv [[-2 0] [2 0] [-1 -1] [1 -1] [-1 1] [1 1]])

(defn neighs [pos]
  (map #(add-vec % pos) nv))

(defn black-neighs [cur pos]
  (->> (neighs pos)
       (map #(cur %))
       (remove nil?)
       count))

(defn solve2 [in n]
  (loop [cur in n n]
    (if (= n 0)
      (count cur)
      (let [new-neighs (clojure.set/difference
                        (->> (map neighs cur)
                             (apply concat)
                             set)
                        cur)
            new-neighs (set (filter #(= (black-neighs cur %) 2) new-neighs))
            new-cur (set (filter #(#{1 2} (black-neighs cur %)) cur))]
        (recur (clojure.set/union new-neighs new-cur) (dec n))))))

(solve2 start 100)
;; => 3964
