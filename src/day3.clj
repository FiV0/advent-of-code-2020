(ns day3
  (:require [clojure.java.io :as io]))

;; part 1

(defn read-input []
  (let [in (-> (slurp (io/resource "input3.txt"))
               (clojure.string/split #"\n"))
        in (map seq in)]
    in))

(def in (read-input))

(defn get-in* [in i j]
  (-> (nth in j)
      (nth i)))

(defn solve [in x y]
  (loop [i 0 j 0 res 0]
    (if (>= j (count in))
      res
      (recur (mod (+ i x) (count (first in)))
             (+ j y)
             (+ res (if (= (get-in* in i j) \#) 1 0))))))

(solve in 3 1)

;; part 2

(->> (map (fn [[x y]]
            (solve in x y))
          '([1 1] [3 1] [5 1] [7 1] [1 2]))
     (apply *))
