(ns lhrb.words.solver
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.protocols :refer [walk]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :refer [difference intersection union]]))

(def alphabet (set (map char (range 97 123))))

(def words
  (with-open [reader (io/reader "resources/words.txt")]
    (doall (line-seq reader))))

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

(defn- build-index-per-letter
  [words f]
  (reduce-kv
   (fn [m k v]
     (assoc m k
            (reduce
             (fn [acc elem]
               (assoc acc elem
                      (set (f #(str/includes? % (str elem)) v))))
             {} alphabet)))
   {}
   (group-by count words)))

(defn build-contains-index
  "* first mapping is the letter count
   * second a mapping letter -> all words which contains the letter"
  [words]
 (build-index-per-letter words filter))

(defn build-without-index
  [words]
  (build-index-per-letter words remove))

(defprotocol WordDB
  (match [this length pos letter] "returns all words, of the given length,
                              that match the letter at the given position")
  (contains [this length letter] "returns all words, that contain the given letter")
  (without [this length letter] "returns all words without the given letter"))

(deftype Words [idx1 idx2 idx3]
  WordDB
  (match [this length pos letter] (get-in idx1 [length pos letter]))
  (contains [this length letter] (get-in idx2 [length letter]))
  (without [this length letter] (get-in idx3 [length letter])))

(def DB (Words. (build-index words)
                (build-contains-index words)
                (build-without-index words)))


;; CORE LOGIC RULES

(defn ismatch
    [w length pos letter]
    (fn [env]
      (let [w (walk env w)]
        (if (lvar? w)
          (to-stream
           (for [word (match DB length pos letter)]
             (unify env w word)))
          env))))

(defn containo
    [w length pos letter]
    (fn [env]
      (let [w (walk env w)]
        (if (lvar? w)
          (to-stream
           (for [word (difference
                       (contains DB length letter)
                       (match DB length pos letter))]
             (unify env w word)))
          env))))

(defn missingo [w length letters]
    (fn [env]
      (let [w (walk env w)]
        (if (lvar? w)
          (to-stream
           (for [word (apply intersection
                             (map #(without DB length %) letters))]
             (unify env w word)))
          env))))


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






(comment
  (def guesses
    [[[:word/miss \s] [:word/miss \i] [:word/miss \c] [:word/miss \h]]
     [[:word/miss \a] [:word/miss \t] [:word/contains \e] [:word/miss \m]]
     [[:word/miss \d] [:word/match \e] [:word/match \n] [:word/match \n]]])

  (def g (fold-guesses guesses))
  (def length (count g))
  (def match-or-contains (reduce (fn [miss elem] (union miss
                                                      (:word/match elem)
                                                      (:word/contains elem)))
                          #{} g))
  (def miss (difference
             (reduce (fn [miss elem] (union miss (:word/miss elem))) #{} g)
             match-or-contains))



  (require '[lhrb.words.core :as c])

  (c/guess "verquer" "kroeter")
  "draco" "maden"

  (run* [w]
     (fresh [w2 w3]
       (ismatch w2 4 1 \m)
       (ismatch w 4 0 \a)
       (containo w3 4 2 \s)
       (== w w2)
       (== w w3)))

  (run* [w]
    (fresh [w1 w2]
      (missingo w1 4 [\a \e \u])
      (containo w2 4 1 \c)
      (== w w1)
      (== w w2)))

  (difference #{1 2} #{1 3})

  (group-by set words)

  (defmacro compile [guesses]
  (let [db (get idx1 7)
        db2 (get idx2 7)
        db3 (get idx3 7)]
    `(run* ['w]
      (fresh [w1 w2]
        (missingo ~db3 w1 [\a \e \u])
        (containo ~db ~db2 w2 1 \c)
        (== w w1)
        (== w w2)))))

  ,)
