(ns day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; part 1

(defn parse-rest [x]
  (->> (map #(take 3 %) (partition 4 (drop 4 x)))
       (map #(cons (Integer/parseInt (first %)) (rest %)))))

(defn read-input []
  (let [in (-> (slurp (io/resource "input7.txt"))
               (str/split #"\n"))
        in (map #(str/split % #" ") in)
        in (map (fn [x]
                  [(take 2 x)
                   (parse-rest x)]) in)]
    in))

(def in (read-input))
(def in-map (into {} in))

(def shiny '("shiny" "gold"))

(defn can-hold [[x r]]
  (if (= x shiny)
    true
    (->> (map rest r)
         (some #(can-hold [% (get in-map %)])))))

(->> (dissoc in-map shiny)
     (map can-hold)
     (remove nil?)
     count)
;; => 246

;; part 2

(def count-bags (memoize (fn [_ r]
                           (cond-> (apply + (map first r))
                             r (+ (->>
                                   (map (fn [[x & rest]]
                                          (* x (count-bags rest (get in-map rest)))) r)
                                   (apply +)))))))

(count-bags shiny (get in-map shiny))
;; => 2976
