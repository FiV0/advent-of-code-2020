(ns day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

;; part 1

(defn read-input []
  (let [in (-> (slurp (io/resource "input12.txt"))
               (str/split #"\n"))
        in (map #(vector (first %) (Integer/parseInt (apply str (rest %)))) in)]
    (vec in)))

(def in (read-input))

(def dirs [\E \S \W \N])
(def dir-to-vec {\E [1 0] \S [0 1] \W [-1 0] \N [0 -1]})

(defn vec+ [[i j] [x y] itensity]
  [(+ i (* x itensity))
   (+ j (* y itensity))])

(defn solve [start in]
  (loop [cur in dir start pos [0 0]]
    (if (seq cur)
      (match (first cur)
        [\F n] (recur (rest cur) dir (vec+ pos (get dir-to-vec (nth dirs dir)) n))
        [\R n] (recur (rest cur) (mod (+ dir (/ n 90)) 4) pos)
        [\L n] (recur (rest cur) (mod (+ dir (/ n -90)) 4) pos)
        [d n]  (recur (rest cur) dir (vec+ pos (get dir-to-vec d) n)))
      (apply + (map #(Math/abs %) pos)))))

(solve 0 in)
;; => 415

;; part 2

(defn rotate [[i j :as waypoint] x]
  (let [x (mod x 4)]
    (case x
      0 waypoint
      1 [(* -1 j) i]
      2 [(* -1 i) (* -1 j)]
      3 [j (* -1 i)])))

(defn solve2 [start in]
  (loop [cur in waypoint start pos [0 0]]
    (if (seq cur)
      (match (first cur)
        [\F n] (recur (rest cur) waypoint (vec+ pos waypoint n))
        [\R n] (recur (rest cur) (rotate waypoint (/ n 90)) pos)
        [\L n] (recur (rest cur) (rotate waypoint (/ n -90)) pos)
        [d n]  (recur (rest cur) (vec+ waypoint (get dir-to-vec d) n) pos))
      (apply + (map #(Math/abs %) pos)))))

(solve2 [10 -1] in)
