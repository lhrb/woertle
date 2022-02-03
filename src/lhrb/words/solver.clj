(ns lhrb.words.solver
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.protocols :refer [walk]]
            [clojure.java.io :as io]))

(def alphabet (set (map char (range 97 123))))

(def words
  (with-open [reader (io/reader "resources/words.txt")]
    (doall (line-seq reader))))

(defn transpose [m]
  (apply mapv vector m))

(defn fold-guesses
  "returns for every letter position a map with keys :word/miss, :word/contains,
  :word/match and as vals a set of the letters"
  [guesses]
  (->> guesses
       (transpose)
       (mapv (fn [xs] (group-by first xs)))
       (mapv (fn [xs]
              (reduce-kv (fn [m k v]
                           (update m k (fnil into #{}) (map second v))) {} xs)))))

(defn build-index
  "returns an index:
  * first mapping is the letter count
  * second the position of the char
  * third the char e.g. {4 {0 {a #{'aber'}}
                             1 {b #{'aber}}}}"
  [words]
  (reduce-kv
   (fn [m k v]
     (assoc m k (reduce
                 (fn [index word]
                   (->> (seq word)
                        (map-indexed (fn [pos letter] [pos letter]))
                        (reduce (fn [m e]
                                  (update-in m e (fnil conj #{}) word))
                                index)))
                 {}
                 v)))
   {}
   (group-by count words)))

(defn ismatch
    [db w pos letter]
    (fn [env]
      (let [w (walk env w)]
        (if (lvar? w)
          (to-stream
           (for [word (get-in db [pos letter])]
             (unify env w word)))
          env))))


(comment
  (def guesses
    [[[:word/miss \s] [:word/miss \i] [:word/miss \c] [:word/miss \h]]
     [[:word/miss \a] [:word/miss \t] [:word/contains \e] [:word/miss \m]]
     [[:word/miss \d] [:word/match \e] [:word/match \n] [:word/match \n]]])


  (require '[lhrb.words.core :as c])

  (c/guess "verquer" "kroeter")
  "draco" "maden"

  (def db (get (build-index words) 4))

  (run* [w]
     (ismatch db w 0 \a)
     (fresh [w2]
       (ismatch db w2 1 \m)
       (== w w2)))


  ,)
