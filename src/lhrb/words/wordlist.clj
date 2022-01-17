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

(defn write-to-file [filename words]
  (with-open [wrtr (io/writer filename)]
    (doseq [i words]
      (.write wrtr (str i "\n")))))

(defn read-file
  ([filename]
   (read-file filename identity))
  ([filename xf]
   (with-open [reader (io/reader filename)]
     (doall (sequence xf (line-seq reader))))))

(comment
  ;; experimental cleanup
  ;; TODO find better word list


  (def dic
    (comp
     (map str/lower-case)
     (map replace-umlauts)
     (filter a-z)
     (filter #(between-4-and-7 (count %)))))

  (def dictonary (read-file "resources/raw/modified-wordlist.txt" dic))

  (def xf
    (comp
     (mapcat #(str/split % #" "))
     (remove empty?)
     (map #(str/replace % #"\.|\,|«|»|\?|!|;|:" ""))
     (map str/lower-case)
     (map replace-umlauts)
     (filter a-z)
     (filter #(between-4-and-7 (count %)))
     (distinct)
     (filter (set dictonary))))

  (def words (read-file "resources/raw/all" xf))

  (write-to-file "resources/words.txt" words)

  (repeatedly 100 #(rand-nth words))



  (str/replace ".,«»asdasd»asdf?fasdf!w" #"\.|\,|«|»|\?|!|;" "")


 *e)
