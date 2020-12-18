(ns day18
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

;; part 1

(defn read-input []
  (let [in (-> (slurp (io/resource "input18.txt"))
               (str/split #"\n"))]
    in))

(def in (read-input))

(defn find-matching [s]
  (loop [s (rest s) cur 1 i 1]
    (cond
      (= cur 0) i
      (= (first s) \() (recur (rest s) (inc cur) (inc i))
      (= (first s) \)) (recur (rest s) (dec cur) (inc i))
      :else      (recur (rest s) cur (inc i)))))

(find-matching "( 123 + 123 * (12 + 12))")

(defn numeric? [c]
  (and (<= (int \0) (int c)) (<= (int c) (int \9))))

(defn first-non-numeric [s]
  (reduce #(if (numeric? %2) (inc %1) (reduced %1)) 0 (seq s)))

(defn parse-one [s]
  (loop [s s res []]
    (cond
      (not (seq s)) res
      (= (first s) \()
      (let [i (find-matching s)]
        (recur (if (>= (inc i) (count s)) "" (subs s (inc i))) (conj res (parse-one (subs s 1 (dec i))))))
      (numeric? (first s))
      (let [i (first-non-numeric s)]
        (recur (subs s i) (conj res (Long/parseLong (subs s 0 i)))))
      (= (first s) \space)
      (recur (subs s 1) res)
      :else
      (recur (subs s 1) (conj res (resolve (symbol (str (first s)))))))))

(parse-one "( 123 + 123 * (12 + 12))")
(parse-one "(8 + 5 + 5) + (8 + (2 * 6) + (6 * 6 * 9 * 8) + 3 + 9 * 8)")

(defn solve-one [p]
  (cond
    (not (coll? p)) p
    (> (count p) 1)
    (solve-one (cons ((second p)
                      (solve-one (first p))
                      (solve-one (nth p 2)))
                     (drop 3 p)))
    :else (solve-one (first p))))

(-> "2 * 3 + (4 * 5)" parse-one solve-one)
(-> "1 + (2 * 3) + (4 * (5 + 6))" parse-one solve-one)

(->> (map #(-> % parse-one solve-one) in)
     (apply +))
;; => 5374004645253

;; part 2

(declare solve-+-first)

(defn +helper [p]
  (cond
    (nil? (second p)) (list (solve-+-first (first p)))
    (= (second p) #'+)
    (+helper (cons (+ (solve-+-first (first p))
                      (solve-+-first (nth p 2)))
                   (drop 3 p)))
    :else
    (cons (solve-+-first (first p)) (cons (second p) (+helper (drop 2 p))))))

(defn solve-+-first [p]
  (cond (not (coll? p)) p
        (> (count p) 1)
        (solve-one (+helper p))
        :else
        (solve-+-first (first p))))

(->> (map #(-> % parse-one solve-+-first) in)
     (apply +))
;; => 88782789402798
