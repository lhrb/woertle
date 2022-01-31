(ns lhrb.words.solver
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.java.io :as io]))

(def alphabet (set (map char (range 97 123))))

(def words
  (with-open [reader (io/reader "resources/words.txt")]
    (doall (line-seq reader))))

(defn not-appearing-letters
  "returns letters which do not appear in the word. Generated from guesses"
  [guesses]
  (->> guesses
       flatten
       (partition 2)
       (filter #(= :word/miss (first %)))
       (map second)))

(defn available-letters
  [not-appearing-letters]
  (clojure.set/difference alphabet (set not-appearing-letters)))

(defn remaining-alph [guesses]
  (->> guesses
       (not-appearing-letters)
       (available-letters)
       (vec)))

(defn transpose [m]
  (apply mapv vector m))

(defn match-or-contains?
  [[x _]]
  (or (= :word/contains x)
      (= :word/match x)))

;; The chosen representation feels cumbersome. But to explore the problem it
;; should be good enough.

(defn guesses-imc
  "imc => index/match/contains
  representation of each index and the information whether we had a match or
  at least the word contains the letter, but on a different position"
  [guesses]
  (->> guesses
       (transpose)
       (map-indexed
        (fn [idx e]
          {:letter/idx idx
           :letter/info (filter match-or-contains? e)}))))

(defn- get-matching-letter [letter-info]
  (second
   (first
    (filter #(= :word/match (first %)) letter-info))))

(defn index-match
  "take the result from guesses-imc and only return index and the matching letter"
  [m]
  (->> m
       (filter (fn [{:letter/keys [info]}]
                 (and (seq info)
                      (some #(= :word/match (first %)) info))))
       (map (fn [{:letter/keys [idx info]}]
              {:letter/idx idx
               :letter/match (get-matching-letter info)}))))

(defn- get-containing-letters
  [letter-info]
  (mapcat (fn [[prop letter]]
            (if (= :word/contains prop)
              (list letter)
              '()))
          letter-info))

(defn index-contains
  [m]
  (->> m
       (filter (fn [{:letter/keys [info]}]
                 (and (seq info)
                      (some #(= :word/contains (first %)) info))))
       (map (fn [{:letter/keys [idx info]}]
              {:letter/idx idx
               :letter/contains (get-containing-letters info)}))))

(defn- get-conde-clause
  "helper to resolve clauses where we know that the word
  'contains' a letter but we don't know the position

  admittedly too many arguments"
  [syms letter in-matches indexes]
  (if (empty? indexes)
    `(conde [succeed])
   (let [clauses (for [i indexes]
                   `[(== ~(syms i) ~letter)])
         clauses (if (in-matches letter)
                   (cons `[succeed] clauses)
                   clauses)]
     `(conde ~@clauses))))

(defn applystr [str-coll]
  (apply str str-coll))

(defn get-contains-clauses
  ":word/contains gives us two informations:
  1. at the position it appears we expect another letter
  2. the letter is at one of the other positions
  we compile this information to a clause in our logic language"
  [syms remaining-indexes in-matches idx-contains]
  (mapcat
   (fn [{:letter/keys [idx contains]}]
     (mapcat
      (fn [letter]
        ;; TODO remove current index from "remaining-indexes"
        (let [conde-clauses (get-conde-clause syms letter in-matches remaining-indexes)]
          `((!= ~(syms idx) ~letter)
            ~conde-clauses)))
      contains))
   idx-contains))

(defmacro compile-to-logic
  [guesses]
  (let [alph (remaining-alph guesses)
        num-letters (count (first guesses))

        db (apply hash-set (filter #(= (count %) num-letters) words))
        xf `(comp (map applystr) (filter ~db))
        ;; logic var wildcards
        syms (vec (for [s (range 0 num-letters)] (gensym s)))
        ;; first we compile the clauses which contain matches.
        idx-match-contains (guesses-imc guesses)
        idx-match (index-match idx-match-contains)
        matches (map (fn [{:letter/keys [idx match]}]
                       `(== ~(syms idx) ~match))
                     idx-match)
        ;; needed for special case 'contains' but is also a match
        ;; TODO find out if still needed?
        in-matches (set (map :letter/match idx-match))

        ;; for all remaining positions we have to check all
        ;; letters from the remaining alphabet
        remaining-indexes (clojure.set/difference
                           (set (range 0 num-letters))
                           (set (map :letter/idx idx-match)))

        letters-to-check (map (fn [idx] `(membero ~(syms idx) ~alph))
                              remaining-indexes)

        idx-contains (index-contains idx-match-contains)
        contains-clauses (get-contains-clauses syms remaining-indexes in-matches idx-contains)]
    `(transduce ~xf conj []
      (run* ~syms
        ~@matches
        ~@contains-clauses
        ~@letters-to-check))))


(comment
  (def guesses
    [[[:word/miss \s] [:word/miss \i] [:word/miss \c] [:word/miss \h]]
     [[:word/miss \a] [:word/miss \t] [:word/contains \e] [:word/miss \m]]
     [[:word/miss \d] [:word/match \e] [:word/match \n] [:word/match \n]]])


  ;; first get all the possible letters.
  (def alph (remaining-alph guesses))
  (def num-letters (count (first guesses)))
  ;; logic var wildcards
  (def syms (vec (for [s (range 0 num-letters)] (gensym s))))
  ;; first we compile the clauses which contain matches.
  (def idx-match-contains (guesses-imc guesses))
  (def idx-match (index-match idx-match-contains))
  (def matches
    (map (fn [{:letter/keys [idx match]}]
                       `(== ~(syms idx) ~match))
         idx-match))

  ;; needed for special case 'contains' but is also a match
  (def in-matches (set (map :letter/match idx-match)))

  ;; for all remaining positions we have to check all
  ;; letters from the remaining alphabet
  (def remaining-indexes
    (clojure.set/difference
     (set (range 0 num-letters))
     (set (map :letter/idx idx-match))))

  (def letters-to-check
    (map (fn [idx] `(membero ~(syms idx) ~alph))
         remaining-indexes))

  (def idx-contains (index-contains idx-match-contains))
  (def contains-clauses (get-contains-clauses syms remaining-indexes in-matches idx-contains))


  (macroexpand-1 '(compile-to-logic
                 [[[:word/miss \s] [:word/miss \i] [:word/miss \c] [:word/miss \h]]
                  [[:word/miss \a] [:word/miss \t] [:word/contains \e] [:word/miss \m]]
                  [[:word/miss \d] [:word/match \e] [:word/match \n] [:word/match \n]]]))


  (compile-to-logic
                 [[[:word/miss \s] [:word/miss \i] [:word/miss \c] [:word/miss \h]]
                  [[:word/miss \a] [:word/miss \t] [:word/contains \e] [:word/miss \m]]
                  [[:word/miss \d] [:word/match \e] [:word/match \n] [:word/match \n]]])


  (require '[lhrb.words.core :as c])

  (c/guess "lodernd" "erleben")

  (macroexpand-1
   '(compile-to-logic [[[:word/miss \s]
                       [:word/miss \c]
                       [:word/miss \h]
                       [:word/contains \n]
                       [:word/miss \a]
                       [:word/miss \t]
                        [:word/miss \z]]]))

  (def res3
    (compile-to-logic [[[:word/miss \s]
                        [:word/miss \c]
                        [:word/miss \h]
                        [:word/contains \n]
                        [:word/miss \a]
                        [:word/miss \t]
                        [:word/miss \z]]
                       [[:word/miss \e]
                        [:word/contains \r]
                        [:word/contains \l]
                        [:word/match \e]
                        [:word/miss \b]
                        [:word/miss \e]
                        [:word/contains \n]]]))


  ,)
