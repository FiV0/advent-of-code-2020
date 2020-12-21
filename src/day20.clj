(ns day20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

;; part 1

(defn read-tile [tile]
  (let [[n & lines] (->> (str/split tile #"\n")
                         (map seq))
        n (Integer/parseInt (apply str (butlast (drop 5 n))))]
    [n lines]))

(defn read-input []
  (let [in (-> (slurp (io/resource "input20.txt"))
               (str/split #"\n\n"))]
    (into {} (map read-tile in))))

(def in (atom (read-input)))
(def ids (keys @in))

(defn rotate [mat]
  (apply map (fn [& args]
               (-> (apply vector args)
                   reverse)) mat))

(defn rotations [mat]
  (take 4 (iterate rotate mat)))

(defn flippings [mat]
  (list mat
        (reverse mat)
        (map reverse mat)
        (reverse (map reverse mat))))

(defn rot-flip [mat]
  (->> (rotations mat)
       (map flippings)
       (apply concat)
       distinct))

(defn above [mat1 mat2]
  (= (last mat1) (first mat2)))

(defn below [mat1 mat2]
  (above mat2 mat1))

(defn left [mat1 mat2]
  (= (map last mat1) (map first mat2)))

(defn right [mat1 mat2]
  (left mat2 mat1))

(def start (list (repeat 3 nil)
                 (list nil (first (first @in)) nil)
                 (repeat 3 nil)))

(defn add-boundary [mat]
  (cond (some (complement nil?) (map first mat))
        (map #(cons nil %) mat)
        (some (complement nil?) (map last mat))
        (map #(concat % '(nil)) mat)
        (some (complement nil?) (first mat))
        (cons (repeat (count (first mat)) nil) mat)
        (some (complement nil?) (last mat))
        (concat mat (list (repeat (count (first mat)) nil)))
        :else
        mat))

(defn vec+ [[i j] [x y]]
  [(+ i x) (+ j y)])

(defn get-mat [mat [i j]]
  (cond
    (< (dec i) 0) nil
    (< (dec j) 0) nil
    (>= (inc i) (count (first mat))) nil
    (>= (inc j) (count mat)) nil
    :else (nth (nth mat j) i)))

(defn set-mat [mat [i j] val]
  (concat (take j mat)
          (list (concat (take i (nth mat j))
                        (list val)
                        (drop (inc i) (nth mat j))))
          (drop (inc j) mat)))

(def dirs [[0 1] [1 0] [0 -1] [-1 0]])

(defn check [current mat pos]
  (let [val (get-mat current pos)
        neighs (map (fn [v] (get-mat current (vec+ pos v))) dirs)]
    (if (and (nil? val) (some (complement nil?) neighs))
      (let [[b r a l] neighs]
        (and (or (nil? b) (below (get @in b) mat))
             (or (nil? r) (left mat (get @in r)))
             (or (nil? a) (below mat (get @in a)))
             (or (nil? l) (left (get @in l) mat))))
      false)))

(defn add [current keys]
  (loop [[key & keys] keys]
    (if (nil? key)
      :error
      (if-let [[res pos] (->> (for [rot (rot-flip (get @in key))
                                    i (range (count (first current)))
                                    j (range (count current))]
                                [rot [i j]])
                              (remove #(number? (get-mat current (second %))))
                              (reduce (fn [_ [rot pos]]
                                        (when (check current rot pos) (reduced [rot pos]))) nil))]
        (do
          (reset! in (assoc @in key res))
          [key (-> (set-mat current pos key)
                   add-boundary)])
        (recur keys)))))

(defn solve [current keys]
  (loop [cur current ks keys]
    (if (seq ks)
      (let [[k new-cur] (add cur ks)]
        (recur new-cur (seq (disj (set ks) k))))
      cur)))

(solve start (rest ids))

(def res *1)
(* (second (second res))
   (last (butlast (second res)))
   (second (last (butlast res)))
   (last (butlast (last (butlast res)))))
;; => 23386616781851
