(ns day1
  (:require [clojure.java.io :as io]))

;; part 1

(defn read-input []
  (let [in (-> (slurp (io/resource "input1.txt"))
               (clojure.string/split #"\n"))
        in (map #(Integer/parseInt %) in)]
    in))

(def in (read-input))
(def in-set (set in))

(reduce (fn [_ e] (when (in-set (- 2020 e)) (reduced (* e (- 2020 e))))) nil in)

;; part 2

(defn cart-2 [c]
  (if (empty? c)
    '()
    (concat (map #(vector (first c) %) (rest c))
            (cart-2 (rest c)))))


(reduce (fn [_ [e1 e2]] (when (in-set (- 2020 e1 e2)) (reduced (* e1 e2 (- 2020 e1 e2))))) nil
        (cart-2 in))
