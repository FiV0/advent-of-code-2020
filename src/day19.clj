(ns day19
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

;; part 1

(defn read-rule [s]
  (let [i (Integer/parseInt (subs s 0 (str/index-of s \:)))
        s (subs s (inc (str/index-of s \:)))
        s (str/split s #"\|")]
    [i
     (map (fn [x]
            (->> (str/split x #" ")
                 (remove str/blank?)
                 (map #(if (str/starts-with? % "\"")
                         (read-string %)
                         (Integer/parseInt %)))
                 vec)) s)]))

(read-rule "0: 1 2 3")
(read-rule "1: 1 2 | 2 1")
(read-rule "1: \"aa\" | 1 2")

(defn read-input []
  (let [[rules strings] (-> (slurp (io/resource "input19.txt"))
                            (str/split #"\n\n"))
        rules (into {} (map-indexed #(read-rule %2) (str/split rules #"\n")))
        strings (str/split strings #"\n")]
    [rules strings]))

(def in (read-input))
(def rules (first in))
(def strings (second in))

(defn cartesian
  ([c1 c2] (cartesian c1 c2 '()))
  ([c1 c2 res]
   (if (and (seq c1) (seq c2))
     (recur (rest c1) c2 (concat res (map #(str (first c1) %) c2)))
     res)))

(def expand
  (memoize
   (fn [i rules]
     (let [rule-i (get rules i)]
       (->> (map #(cond (> (count %) 1)
                        (cartesian (expand (first %) rules)
                                   (expand (second %) rules))
                        (int? (first %))
                        (expand (first %) rules)
                        :else %) rule-i)
            (apply concat))))))

(def generated (set (expand 0 rules)))

(count (filter generated strings))
;; => 149

;; part 2

(def expand2
  (memoize
   (fn [i rules]
     (let [rule-i (get rules i)]
       (cond (= i 8)
             (->> (expand2 (first (first rule-i)) rules)
                  (map #(str % "8")))
             (= i 11)
             (->> (cartesian (->> (expand2 (first (first rule-i)) rules)
                                  (map #(str % "11")))
                             (expand2 (second (first rule-i)) rules)))
             :else (->> (map #(cond (> (count %) 1)
                                    (cartesian (expand2 (first %) rules)
                                               (expand2 (second %) rules))
                                    (int? (first %))
                                    (expand2 (first %) rules)
                                    :else %) rule-i)
                        (apply concat)))))))

(defn split-at [s]
  (let [i (str/index-of s "8")
        j (str/index-of s "11")]
    (cond i
          (cond->> (cons "8" (split-at (subs s (inc i))))
            (pos? i) (cons (subs s 0 i)))
          j
          (cond->> (cons "11" (split-at (subs s (+ j 2))))
            (pos? j) (cons (subs s 0 j)))
          :else
          (list s))))

(split-at "811")
(split-at "aa8bb11cc")
(split-at "aa811cc")
(split-at "811cc")

(def generated2 (->> (expand2 0 rules)
                     (filter #(or (str/includes? % "8")
                                  (str/includes? % "11")))
                     (map #(split-at %))))

(def remaining (remove generated strings))

(defn start-with-set [set s]
  (->> (map #(vector (str/starts-with? s %) (count %)) (seq set))
       (filter #(first %))
       first
       second))

(start-with-set '("ab" "bc" "cd") "cde")
(start-with-set '("ab" "bc" "cd") "efd")

(defn check [s pattern]
  (cond
    (and (str/blank? s) (empty? pattern))
    true
    (empty? pattern)
    false
    (= "8" (first pattern))
    (or (check s (rest pattern))
        (when-let [n (start-with-set (expand2 42 rules) s)]
          (check (subs s n) pattern)))
    (= "31" (first pattern))
    (when-let [n (start-with-set (expand2 31 rules) s)]
      (check (subs s n) (rest pattern)))
    (= "11" (first pattern))
    (or (check s (rest pattern))
        (when-let [n (start-with-set (expand2 42 rules) s)]
          (check (subs s n) (cons "11" (cons "31" (rest pattern))))))
    :else
    (and (str/starts-with? s (first pattern))
         (check (subs s (count (first pattern))) (rest pattern)))))

(defn check2 [s]
  (->> (map #(check s %) generated2)
       (some true?)))

(def r (map-indexed #(do (println %1)
                         (check2 %2)) remaining))

(->> (doall r)
     (remove nil?)
     count
     (+ 149))
;; => 332
