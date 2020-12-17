(ns day17
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

;; part 1

(defn read-input []
  (let [in (-> (slurp (io/resource "input17.txt"))
               (str/split #"\n"))
        in (map-indexed
            (fn [i in]
              (->> (map-indexed
                    (fn [j c]
                      (if (= \# c)
                        [0 j]
                        nil)) (seq in))
                   (remove nil?)
                   (map #(conj % i))))
            in)
        in (apply concat in)]
    (set in)))

(def in (read-input))

(defn flatten-one [x]
  (apply concat x))

(defn expansion [[a b c]]
  (for [x (range -1 2)
        y (range -1 2)
        z (range -1 2)]
    [(+ a x) (+ b y) (+ c z)]))

(defn count-nb [ex-f system coordinate]
  (->> (ex-f coordinate)
       (remove #(= % coordinate))
       (map system)
       (remove nil?)
       count))

(defn iterate-system [ex-f system ]
  (let [expansion (clojure.set/difference (-> (map ex-f (seq system))
                                              flatten-one
                                              set)
                                          system)]
    (clojure.set/union
     (set (filter #(#{2 3} (count-nb ex-f system %)) (seq system)))
     (set (filter #(= 3 (count-nb ex-f system %)) (seq expansion))))))

(->> (iterate (partial iterate-system expansion) in)
     (drop 6)
     first
     count)
;; => 265

;; part 2

(defn read-input2 []
  (let [in (-> (slurp (io/resource "input17.txt"))
               (str/split #"\n"))
        in (map-indexed
            (fn [i in]
              (->> (map-indexed
                    (fn [j c]
                      (if (= \# c)
                        [0 0 j]
                        nil)) (seq in))
                   (remove nil?)
                   (map #(conj % i))))
            in)
        in (apply concat in)]
    (set in)))

(def in2 (read-input2))

(defn expansion2 [[a b c d]]
  (for [x (range -1 2)
        y (range -1 2)
        z (range -1 2)
        o (range -1 2)]
    [(+ a x) (+ b y) (+ c z) (+ d o)]))

(->> (iterate (partial iterate-system expansion2) in2)
     (drop 6)
     first
     count)
;; => 1936
