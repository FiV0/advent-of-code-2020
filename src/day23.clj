(ns day23
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

(def in (->> (seq "469217538")
             (map int)
             (map #(- % (int \0)))))

(defn index-of [l e]
  (first (keep-indexed #(when (= e %2) %1) l)))

(index-of '(1 2 3 4) 3)

(defn next-item [i s]
  (if (s i)
    i
    (next-item (mod (+ (if (= i 1) 9 (dec i)) 10) 10) s)))

(defn solve [in n]
  (loop [n n cur in]
    (if (= n 0)
      (let [i (index-of cur 1)]
        (concat (drop (inc i) cur) (take i cur)))
      (let [[a & r] (take 4 cur)
            remain (drop 4 cur)
            i (->> (next-item a (set remain))
                   (index-of remain))]
        (recur
         (dec n)
         (concat (take (inc i) remain)
                 r
                 (drop (inc i) remain)
                 (list a)))))))

(->> (solve in 100)
     (apply str))

;; part 2

(defn create-mappings [in]
  (loop [in in res {(last in) (first in)}]
    (if (= (count in) 1)
      res
      (recur (rest in) (assoc res (first in) (second in))))))

(def in2 (-> (concat in (range 10 1000001))
             create-mappings))

(defn next-item2 [i s]
  (if (s i)
    (next-item2 (mod (+ (if (= i 1) 1000000 (dec i)) 1000001) 1000001) s)
    i))

(defn solve2 [in start n]
  (loop [n n cur start res in]
    (if (= n 0)
      res
      (let [a1 (get res cur)
            a2 (get res a1)
            a3 (get res a2)
            nx (next-item2 (if (= cur 1) 1000000 (dec cur)) #{a1 a2 a3})
            new-res (-> res
                        (assoc cur (get res a3))
                        (assoc nx a1)
                        (assoc a3 (get res nx)))]
        (recur (dec n) (get new-res cur) new-res)))))

(def res (solve2 in2 4 10000000))

(let [a1 (get res 1)
      a2 (get res a1)]
  (* a1 a2))
;; => 18930983775
