(ns day8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; part 1

(defn read-input []
  (let [in (-> (slurp (io/resource "input8.txt"))
               (str/split #"\n"))
        in (map #(str/split % #" ") in)
        in (map #(vector (first %) (Integer/parseInt (second %))) in)]
    (vec in)))

(def in (read-input))

(defn run [prgm]
  (loop [seen #{} i 0 acc 0]
    (if (seen i)
      acc
      (let [[inst j] (nth prgm i)]
        (case inst
          "acc" (recur (conj seen i) (inc i) (+ acc j))
          "jmp" (recur (conj seen i) (+ i j) acc)
          "nop" (recur (conj seen i) (inc i) acc))))))

(run in)

;; part 2

(defn run2 [prgm]
  (loop [seen #{} i 0 acc 0]
    (cond
      (>= i (count prgm)) acc
      (seen i) :invalid
      :else (let [[inst j] (nth prgm i)]
              (case inst
                "acc" (recur (conj seen i) (inc i) (+ acc j))
                "jmp" (recur (conj seen i) (+ i j) acc)
                "nop" (recur (conj seen i) (inc i) acc))))))

(def replacement {"jmp" "nop" "nop" "jmp"})

(defn find-corrupted [prgm]
  (loop [i 0]
    (let [[inst j] (nth prgm i)]
      (if (= inst "acc")
        (recur (inc i))
        (let [res (run2 (assoc prgm i [(replacement inst) j]))]
          (if (number? res) res (recur (inc i))))))))

(find-corrupted in)
