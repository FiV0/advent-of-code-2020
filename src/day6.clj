(ns day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; part 1
(defn read-input []
  (let [in (-> (slurp (io/resource "input6.txt"))
               (str/split #"\n\n"))
        in (map #(->> (str/split % #"\n")
                      (apply concat)) in)]
    in))

(def in (read-input))

(->> (map #(-> % set count) in)
     (apply +))

;; part 2
(defn read-input []
  (let [in (-> (slurp (io/resource "input6.txt"))
               (str/split #"\n\n"))
        in (map #(->> (str/split % #"\n")
                      (map (fn [x] (set x)))
                      (reduce clojure.set/intersection)) in)]
    in))

(def in (read-input))

(->> (map count in)
     (apply + ))
