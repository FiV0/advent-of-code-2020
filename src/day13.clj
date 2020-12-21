(ns day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

;; part 1

(defn read-input []
  (let [in (-> (slurp (io/resource "input13.txt"))
               (str/split #"\n"))
        in [(Integer/parseInt (first in))
            (->> (str/split (second in) #",")
                 (remove #(= % "x"))
                 (map #(Integer/parseInt %)))]]
    (vec in)))

(def in (read-input))

(->> (reduce (fn [[cur id] x] (if (> cur (- x (mod (first in) x)))
                                [(- x (mod (first in) x)) x]
                                [cur id])) [Integer/MAX_VALUE nil] (second in))
     (apply * ))

;; part 2
(defn read-input []
  (let [in (-> (slurp (io/resource "input13c.txt"))
               (str/split #"\n"))
        in (->> (str/split (second in) #",")
                (map-indexed #(vector %2 %1))
                (remove #(= (first %) "x"))
                (map #(vector (bigint (Integer/parseInt (first %))) (second %))))]
    (vec in)))

(def in2 (read-input))

(defn gcd [x y]
  (if (= y 0)
    x
    (gcd y (mod x y))))

(defn extended-gcd [a b]
  (loop [s 0 t 1 r b old-s 1 old-t 0 old-r a]
    (if (= r 0)
      [old-r old-s old-t]
      (let [q (quot old-r r)]
        (recur (- old-s (* q s)) (- old-t (* q t)) (- old-r (* q r)) s t r)))))

(defn lcm [x y]
  (/ (* x y) (gcd x y)))
