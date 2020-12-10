(ns day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; part 1

(defn read-input []
  (let [in (-> (slurp (io/resource "input10.txt"))
               (str/split #"\n"))
        in (map #(Long/parseLong %) in)]
    (vec in)))

(def in (let [in (vec (sort (cons 0 (read-input))))]
          (conj in (+ 3 (last in)))))

(def res (->> (map (juxt #(= (inc %1) %2) #(= (+ %1 3) %2)) in (rest in))
              (reduce (fn [[res1 res2] [x y]] (cond x [(inc res1) res2]
                                                    y [res1 (inc res2)]
                                                    :else [res1 res2])) [0 0])))

(apply * res)
;; => 2244

;; part 2

(def in-set (set in))

(def solve (memoize (fn [i]
                      (cond-> (if (= i 0) 1 0)
                        (and (>= (dec i) 0) (in-set (dec i))) (+ (solve (dec i)))
                        (and (>= (- i 2) 0) (in-set (- i 2))) (+ (solve (- i 2)))
                        (and (>= (- i 3) 0) (in-set (- i 3))) (+ (solve (- i 3)))))))

(solve (last in))
;; => 3947645370368
