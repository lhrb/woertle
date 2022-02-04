(ns lhrb.words.solver
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.protocols :refer [walk]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :refer [difference intersection]]))

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

(def ^:dynamic *db* (build-index words))
(def ^:dynamic *db2* (build-contains-index words))
(def ^:dynamic *db3* (build-without-index words))

(defn ismatch
    [w wlc pos letter]
    (fn [env]
      (let [w (walk env w)]
        (if (lvar? w)
          (to-stream
           (for [word (get-in *db* [wlc pos letter])]
             (unify env w word)))
          env))))

(defn containo
    [w wlc pos letter]
    (fn [env]
      (let [w (walk env w)]
        (if (lvar? w)
          (to-stream
           (for [word (difference
                       (get-in *db2* [wlc letter])
                       (get-in *db* [wlc pos letter]))]
             (unify env w word)))
          env))))

(defn missingo [w wlc letters]
    (fn [env]
      (let [w (walk env w)]
        (if (lvar? w)
          (to-stream
           (for [word (apply intersection
                             (map #(get-in *db3* [wlc %]) letters))]
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
*db2*
  (difference
   (get-in db2 [\a])
   (get-in *db* [0 \a]))

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
