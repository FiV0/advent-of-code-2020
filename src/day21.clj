(ns day21
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

;; part 1

(defn parse-food [s]
  (let [[ingrediants allergens] (str/split s #"\(")
        ingrediants (str/split ingrediants #" ")
        allergens (when allergens (-> (subs allergens 0 (dec (count allergens)))
                                      (str/split #" ")
                                      rest))
        allergens (map #(if (= (last %) \,) (subs % 0 (dec (count %))) %) allergens)]
    [ingrediants allergens]))

(parse-food "a b c (contains soy, fish)")
(parse-food "a b c ")
(parse-food "a b c (contains foo)")

(defn read-input []
  (let [in (-> (slurp (io/resource "input21.txt"))
               (str/split #"\n"))]
    (vec (map parse-food in))))

(def in (read-input))

(defn solve-helper [in]
  (loop [res {} cur in]
    (if (seq cur)
      (let [[ing all] (first cur)]
        (recur
         (reduce (fn [res allergen]
                   (if (contains? res allergen)
                     (update res allergen clojure.set/intersection (set ing))
                     (assoc res allergen (set ing)))) res all)
         (rest cur)))
      res)))

(defn solve [in]
  (let [res (solve-helper in)
        res (->> res vals (reduce clojure.set/union))]
    (reduce (fn [cnt ing]
              (+ cnt (count (clojure.set/difference (set ing) res)))) 0 (map first in))))

(solve in)
;; => 2412

;; part 2

(def all->possible-ingrediants (solve-helper in))

(defn solve2 [in]
  (loop [cur in res {}]
    (if (seq cur)
      (let [[all ing] (reduce-kv (fn [_ all ings] (when (= (count ings) 1) (reduced [all (first ings)]))) nil cur)]
        (recur (->> (dissoc cur all)
                    (map (fn [[k v]] [k (disj v ing)]))
                    (into {}))
               (assoc res all ing)))
      res)))

(->> (solve2 all->possible-ingrediants)
     (sort (fn [x y] (compare (first x) (first y))))
     (map second)
     (str/join ","))
;; => "mfp,mgvfmvp,nhdjth,hcdchl,dvkbjh,dcvrf,bcjz,mhnrqp"
