(ns day24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

;; part 1

(defn read-input []
  (let [in (-> (slurp (io/resource "input25.txt"))
               (str/split #"\n"))]
    (map #(Integer/parseInt %) in)))

(def in (read-input))
(def divider 20201227)

(defn solve [sub target]
  (loop [i 0 cur sub]
    (if (= cur target)
      i
      (recur (inc i) (rem (* cur sub) divider)))))

(defn solve2 [n start]
  (loop [i 0 cur start]
    (if (= i n)
      cur
      (recur (inc i) (rem (* cur start) divider)))))


(def publics (map #(solve 7 %) in))
(solve2 (first publics) (second in))
;; => 3015200
