(ns lhrb.words.wordlist
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :refer [superset?]]))

(def alphabet (set (map char (range 97 123))))

(defn between-4-and-7 [x]
  (and (<= 4 x)
       (<= x 7)))

(defn replace-umlauts [s]
  (-> s
      (str/replace #"ö" "oe")
      (str/replace #"ä" "ae")
      (str/replace #"ü" "ue")
      (str/replace #"ß" "ss")))

(defn a-z [s]
  (superset? alphabet (set s)))

(comment
 ;; experimental cleanup
 ;; TODO find better word list

 (def words
   (with-open [reader (io/reader "resources/raw/hpwords.txt")]
     (doall
      (->> (line-seq reader)
           (filter #(between-4-and-7 (count %)))
           (map str/lower-case)
           (map replace-umlauts)
           (filter a-z)
           (filter #(between-4-and-7 (count %)))))))

 (count words)

 (with-open [wrtr (io/writer "resources/words.txt")]
   (doseq [i words]
     (.write wrtr (str i "\n"))))

 *e)
