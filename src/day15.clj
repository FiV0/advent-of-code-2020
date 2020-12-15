(ns day15
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

;; part 1

(def in [8,13,1,0,18,9])

(defn solve [in n]
  (loop [seen (into {} (map-indexed (fn [i e] [e (inc i)]) (butlast in))) prev (last in) i (count in)]
    (let [res (if (contains? seen prev)
                (- i (get seen prev))
                0)]
      (if (= (inc i) n)
        res
        (recur (assoc seen prev i) res (inc i))))))

(solve in 2020)
;; => 755

;; part 2

(solve in 30000000)
;; => 11962
