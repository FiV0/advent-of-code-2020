(ns day14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

;; part 1

(defn parse-mask [s]
  (-> (str/split s #" = ")
      second
      seq))

(parse-mask "mask = 0X0X1110X1010X1X10010X0011010X100110")

(defn parse-mem [s]
  (let [[mem num] (str/split s #" = ")]
    [(Long/parseLong (subs mem 4 (dec (count mem))))  (Long/parseLong num)]))

(parse-mem "mem[40190] = 23031023")

(defn read-input []
  (let [in (-> (slurp (io/resource "input14.txt"))
               (str/split #"\n"))
        in (map #(if (str/starts-with? % "mask")
                   (parse-mask %)
                   (parse-mem %)) in)]
    (vec in)))

(def in (read-input))

(defn apply-mask [n mask]
  (let [mask (reverse mask)]
    (loop [res n i 0 m mask]
      (if (seq m)
        (case (first m)
          \X (recur res (inc i) (rest m))
          \0 (recur (bit-clear res i) (inc i) (rest m))
          \1 (recur (bit-set res i) (inc i) (rest m)))
        res))))

(defn solve [in]
  (loop [mask (first in) [ele :as in] (rest in) mem {}]
    (cond (not ele)
          (reduce-kv (fn [res _ v] (+ res v)) 0 mem)
          (char? (first ele))
          (recur ele (rest in) mem)
          :else
          (recur mask (rest in) (assoc mem (first ele) (apply-mask (second ele) mask))))))

(solve in)
;; => 6631883285184

;; part 2

(defn padded-int [n]
  (let [res (Long/toString n 2)]
    (if (< (count res) 36)
      (str (apply str (repeat (- 36 (count res)) \0))
           res)
      res)))

(defn apply-mask2 [n mask]
  (let [n-mask (seq (padded-int n))]
    (map (fn [c1 c2]
           (case c2
             (\X \1) c2
             c1))
         n-mask mask)))

(def combine (memoize (fn [mask]
                        (if (seq mask)
                          (if (= (first mask) \X)
                            (concat (map #(cons \0 %) (combine (rest mask)))
                                    (map #(cons \1 %) (combine (rest mask))))
                            (map #(cons (first mask) %) (combine (rest mask))))
                          '(())))))

(defn forwards-solve [in]
  (loop [mask (first in) [ele :as in] (rest in) mem {}]
    (cond (not ele)
          (reduce-kv (fn [res _ v] (+ res v)) 0 mem)
          (char? (first ele))
          (recur ele (rest in) mem)
          :else
          (recur mask (rest in) (reduce (fn [mem x] (assoc mem x (second ele))) mem (combine (apply-mask2 (first ele) mask)))))))

(forwards-solve in)
;; => 3161838538691
