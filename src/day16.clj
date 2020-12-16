(ns day16
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

;; part 1

(defn parse-class [s]
  (let [[class values] (str/split s #":")
        values (->> (str/split values #" or ")
                    (map (fn [x]
                           (->> (str/split x #"-")
                                (map str/trim)
                                (map #(Integer/parseInt %))
                                vec))))]
    [class values]))

(parse-class "departure location: 47-164 or 179-960")

(defn parse-int-list [s]
  (->> (str/split s #",")
       (map #(Integer/parseInt %))))

(defn read-input []
  (let [[classes my-ticket tickets]
        (-> (slurp (io/resource "input16.txt"))
            (str/split #"\n\n"))
        classes (->> (str/split classes #"\n")
                     (map parse-class ))
        my-ticket (-> (str/split my-ticket #"\n") second parse-int-list)
        tickets (->> (str/split tickets #"\n")
                     rest
                     (map parse-int-list))]
    [classes my-ticket tickets]))

(def in (read-input))

(defn solve [in]
  (let [intervals (->> (map second (first in))
                       (reduce concat))
        values (flatten (last in))]
    (->> (remove #(some (fn [[x y]] (and (<= x %) (<= % y))) intervals) values)
         (reduce +))))

(solve in)
;; => 27802

;; part 2

(defn discard [in]
  (let [intervals (->> (map second (first in))
                       (reduce concat))
        tickets (last in)]
    (filter (fn [ticket]
              (= (count ticket)
                 (count (filter #(some (fn [[x y]] (and (<= x %) (<= % y))) intervals) ticket))))
            tickets)))

(def valid-tickets (discard in))

(def my-ticket (second in))
(def all-tickets (cons (second in) valid-tickets ))
(def fields (first in))

(defn in-range [[[x1 y1] [x2 y2]] nb]
  (or (and (<= x1 nb) (<= nb y1))
      (and (<= x2 nb) (<= nb y2))))

(defn determine-row [nbs fields]
  (reduce (fn [fields nb]
            (filter #(in-range (second %) nb) fields)) fields nbs))

(defn transpose [mat]
  (apply map vector mat))

(defn solve2 [tickets fields]
  (loop [tickets (transpose tickets) i 0 res {}]
    (if-not (seq tickets)
      res
      (recur (rest tickets) (inc i) (assoc res i (determine-row (first tickets) fields))))))

(def mapping (->> (solve2 all-tickets fields)
                  (map (fn [[v fields]]
                         [v (map first fields)]))))

(defn solve-mapping [mapping]
  (if (seq mapping)
    (let [[[n [type]] & rest]
          (sort (comparator #(< (count (second %1))
                                (count (second %2))))
                mapping)]
      (cons [n type] (solve-mapping (map (fn [[v fields]] [v (remove #(= type %) fields)]) rest))))
    '()))

(->> (solve-mapping mapping)
     (filter #(str/starts-with? (second %) "departure"))
     (map first)
     (map #(nth my-ticket %))
     (apply *))
