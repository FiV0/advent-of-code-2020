(ns day22
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

;; part 1

(defn parse-player [s]
  (->> (str/split s #"\n")
       rest
       (map #(Integer/parseInt %))))

(defn read-input []
  (let [in (-> (slurp (io/resource "input22.txt"))
               (str/split #"\n\n"))]
    (map parse-player in)))

(def in (read-input))

(defn solve [[a b]]
  (loop [a (into clojure.lang.PersistentQueue/EMPTY a)
         b (into clojure.lang.PersistentQueue/EMPTY b)]
    (cond (or (empty? a) (empty? b))
          [(seq a) (seq b)]
          (> (peek a) (peek b))
          (recur (-> (pop a)
                     (conj (peek a))
                     (conj (peek b)))
                 (pop b))
          :else
          (recur (pop a)
                 (-> (pop b)
                     (conj (peek b))
                     (conj (peek a)))))))

(def res (solve in))

(->> (first res)
     reverse
     (map-indexed (fn [i x] (* (inc i) x)))
     (apply +))
;; => 31629

;; part 2

(def solve2-helper
  (memoize
   (fn [a b seen]
     (let [item-a (peek a)
           item-b (peek b)]
       (cond (or (empty? a) (empty? b))
             [(seq a) (seq b)]
             (contains? seen [a b])
             [(seq a) nil]
             (and (> item-a item-b)
                  (or (< (count (pop a)) item-a)
                      (< (count (pop b)) item-b)))
             (solve2-helper (-> (pop a)
                                (conj item-a)
                                (conj item-b))
                            (pop b)
                            (conj seen [a b]))
             (and (< item-a item-b)
                  (or (< (count (pop a)) item-a)
                      (< (count (pop b)) item-b)))
             (solve2-helper (pop a)
                            (-> (pop b)
                                (conj item-b)
                                (conj item-a))
                            (conj seen [a b]))
             :else
             (let [[win-a _] (solve2-helper (pop a) (pop b) #{})]
               (if win-a
                 (solve2-helper (-> (pop a)
                                    (conj item-a)
                                    (conj item-b))
                                (pop b)
                                (conj seen [a b]))
                 (solve2-helper (pop a)
                                (-> (pop b)
                                    (conj item-b)
                                    (conj item-a))
                                (conj seen [a b])))))))))

(def mem (atom {}))

(defn solve2 [a b]
  (loop [a a b b seen #{}]
    (if (contains? @mem [a b])
      (get @mem [a b])
      (let [item-a (peek a)
            item-b (peek b)]
        (cond (or (empty? a) (empty? b))
              (swap! mem assoc [a b] [(seq a) (seq b)])
              (contains? seen [a b])
              (swap! mem assoc [a b] [(seq a) nil])
              (and (> item-a item-b)
                   (or (< (count (pop a)) item-a)
                       (< (count (pop b)) item-b)))
              (recur (-> (pop a)
                         (conj item-a)
                         (conj item-b))
                     (pop b)
                     (conj seen [a b]))
              (and (< item-a item-b)
                   (or (< (count (pop a)) item-a)
                       (< (count (pop b)) item-b)))
              (recur (pop a)
                     (-> (pop b)
                         (conj item-b)
                         (conj item-a))
                     (conj seen [a b]))
              :else
              (let [[win-a _ :as res] (solve2 (pop a) (pop b))]
                (swap! mem assoc [(pop a) (pop b)] res)
                (if win-a
                  (recur (-> (pop a)
                             (conj item-a)
                             (conj item-b))
                         (pop b)
                         (conj seen [a b]))
                  (recur (pop a)
                         (-> (pop b)
                             (conj item-b)
                             (conj item-a))
                         (conj seen [a b])))))))))

(def res2 (solve2 (into clojure.lang.PersistentQueue/EMPTY (first in))
                  (into clojure.lang.PersistentQueue/EMPTY (second in))))

(->> (second res2)
     reverse
     (map-indexed (fn [i x] (* (inc i) x)))
     (apply +))
;; => 291
