(ns day2
  (:require [clojure.java.io :as io]))

;; part 1

(defn read-input []
  (let [in (-> (slurp (io/resource "input2.txt"))
               (clojure.string/split #"\n"))
        in (map (fn [s]
                  (let [[n l pw] (clojure.string/split s #" ")
                        [low high] (map #(Integer/parseInt %)(clojure.string/split n #"-"))
                        l (first l)]
                    [[low high] l pw])) in)]
    in))

(def in (read-input))

(-> (filter (fn [[[l h] x pw :as input]]
              (let [n (get (frequencies (seq pw)) x -1)]
                (and (<= l n) (<= n h)))) in)
    count)

;; part 2

(-> (filter (fn [[[l h] x pw]]
              (let [l (nth (seq pw) (dec l))
                    h (nth (seq pw) (dec h))
                    s (into #{} (list l h))]
                (and (= 2 (count s))
                     (s x)))) in)
    count)
