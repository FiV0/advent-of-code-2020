(ns day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; part 1

(defn partition-by* [in]
  (loop [in in acc '() cur []]
    (cond (empty? in)
          (reverse (cons cur acc))
          (= (first in) (list [""]))
          (recur (rest in) (cons cur acc) [])
          :else
          (recur (rest in) acc (concat cur (first in))))))

(defn read-input []
  (let [in (-> (slurp (io/resource "input4.txt"))
               (clojure.string/split #"\n"))
        in (map #(->> (str/split % #" ")
                      (map (fn [s] (str/split s #":")))) in)]
    (partition-by* in)
    ;; in
    ))

(def in (read-input))


;; part 1
(def valids ["ecl" "pid" "eyr" "hcl" "byr" "iyr" "hgt"])

(->> (map #(filter (set valids) (map first %)) in)
     ;; (map set)
     (map count)
     (filter #(= % 7))
     count
     )

;; part 2

(defn digit? [x]
  (and
   (<= (int \0) (int x))
   (<= (int x) (int \9))))

(defn hex? [x]
  (or (digit? x)
      (and
       (<= (int \a) (int x))
       (<= (int x) (int \f)))))

(def valids [["ecl" (fn [x]
                      ((set ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"]) x))]
             ["pid" (fn [x]
                      (let [s (seq x)]
                        (and (= (count s) 9)
                             (every? digit? s))))]
             ["eyr" (fn [x]
                      (let [i (Integer/parseInt x)]
                        (and (<= 2020 i) (<= i 2030))))]
             ["hcl" (fn [x]
                      (let [s (seq x)]
                        (and (= (first s) \#)
                             (every? hex? (rest s)))))]
             ["byr" (fn [x]
                      (let [i (Integer/parseInt x)]
                        (and (<= 1920 i) (<= i 2002))))]
             ["iyr" (fn [x]
                      (let [i (Integer/parseInt x)]
                        (and (<= 2010 i) (<= i 2020)))) ]
             ["hgt" (fn [x]
                      (let [l (count x)]
                        (if (and (<= 4 l) (<= l 5))
                          (let [type (subs x (- l 2))]
                            (case type
                              "in" (let [n (Integer/parseInt (subs x 0 (- l 2)))]
                                     (and (<= 59 n) (<= n 76)))
                              "cm" (let [n (Integer/parseInt (subs x 0 (- l 2)))]
                                     (and (<= 150 n) (<= n 193)))
                              false))
                          false)))]])

(def vs (into {} valids))

(->> (map #(filter (fn [[x y]]
                     (if-let [f (get vs x)]
                       (f y))) %) in)
     (map count)
     (filter #(= % 7)) count)
