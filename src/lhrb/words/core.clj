(ns lhrb.words.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.params :as params]
            [ring.middleware.session :as session]
            [reitit.ring.middleware.muuntaja :as muuntaja]
            [muuntaja.core :as m]
            [reitit.ring.coercion :as coercion]
            [reitit.ring :as ring]
            [hiccup.core :as html])
  (:import [org.eclipse.jetty.server.handler.gzip GzipHandler])
  (:gen-class))

(def words
  (with-open [reader (io/reader "resources/words.txt")]
    (doall (line-seq reader))))

(defn word-in-list [words word]
  ((set words) word))

(def word-in-list? (partial word-in-list words))

(defn won? [asession]
  (->> (last (:session/guesses asession))
       (map second)
       (apply str)
       (= (:session/word asession))))

(defn points-for-round
  "Calculate the points reached for the number of guesses."
  [guess-count]
  (max 5 ;; five points just for guessing right
       (- 100 (* 10
                 (max 0 (dec guess-count))))))
(comment
  (= (points-for-round -1) 100)
  (= (points-for-round 11) 5)

  *e)

(defn compare-letter-position
  "compares the letter at a specific position"
  [letters original guess]
  (cond
    (= original guess)  [:word/match guess]
    (letters guess)     guess
    :else               [:word/miss guess]))

(defn- count-matches
  [current-letter matches]
  (->> matches
       (filter vector?)
       (filter (fn [[_ letter]] (= current-letter letter)))
       (count)))

(defn guess
  "returns how many letters matched (:word/match), are contained (:word/contains)
  in the word or not in the word at all (:word/miss)"
  [word guessw]
  (let [freq (frequencies word)
        comp (partial compare-letter-position (set word))
        ;; first we check for exact matches and misses
        matches (mapv (fn [x y] (comp x y))
                      (seq word) (seq guessw))]
    ;; When we found a letter twice but its only in the word once
    ;; we don't want to mark it as contains.
    (reduce (fn [acc elem]
              (if (vector? elem)
                (conj acc elem)
                (conj acc
                      (if (< 0
                             (- (get freq elem)
                                (count-matches elem (concat matches acc))))
                        [:word/contains elem]
                        [:word/miss elem]))))
            [] matches)))

;; view ------------------------------------------

(defn page [content]
  [:html
   {:lang "de"}
   [:head
    [:meta {:charset "UTF-8"}]
    [:meta {:name "viewport"
            :content "width=device-width, initial-scale=1"}]
    [:link {:rel "stylesheet" :href "css/normalize.css"}]
    [:link {:rel "stylesheet" :href "css/skeleton.css"}]
    [:link {:rel "stylesheet" :href "css/custom.css"}]
    [:title "Wörtle"]]
   [:body
    [:div {:class "container"}
     [:section {:class "header"}
      [:h2 "Wörtle"]]
     content]]])

(defn row [aguess]
  [:word
   (for [[diff letter] aguess]
     [:letter-box {:class (case diff
                            :word/match "match"
                            :word/contains "contains"
                            :word/miss "miss")}
      [:letter (str letter)]])])

(defn instruction-view []
  [:div {:class "info-box mtop-50"}
   [:h3 "Anleitung"]
   [:p "Versuche das Wort zu erraten. Dabei helfen dir die Farben:"]
   [:word2 [:letter-box {:class "match"} [:letter "a"]][:p "Der Buchstabe kommt im Wort vor und ist an der richtigen Stelle."]]
   [:word2 [:letter-box {:class "contains"} [:letter "a"]][:p "Der Buchstabe kommt im Wort vor, steht aber an der falsche Stelle."]]
   [:word2 [:letter-box {:class "miss"} [:letter "a"]][:p "Der Buchstabe kommt leider gar nicht vor"]]
   [:br]
   [:p "Fun fact: Alle Wörter kommen aus den Harry Potter Büchern!"]])

(defn stats-view [asession]
  (let [{{:stats/keys [points guesses words]} :session/stats} asession]
    [:div {:class "stat-box row"}
     [:div {:class "four columns"} (str "Punkte: " points)]
     [:div {:class "four columns"} (str "Versuche insgesamt: " guesses)]
     [:div {:class "four columns"} (str "Erratene Wörter: " words)]]))

(defn get-page [asession]
  (html/html
   (doall
    (page
     [:div
      [:center
       (row (mapv vector
                  (repeat :word/miss)
                  (range 1 (inc (count (:session/word asession))))))
       (for [aguess (:session/guesses asession)]
         (row aguess))

       (if (won? asession)
         [:form {:class "mtop-50" :action "/reset" :method "post"}
          [:input {:class "button-primary" :type "submit" :value "reset"}]]

         [:form {:class "mtop-50" :action "/guess" :method "post"}
          (let [length (str (count (:session/word asession)))]
            [:input {:type "text"
                     :maxlength length
                     :minlength length
                     :name "guess"}])
          [:input {:class "button-primary" :type "submit" :value "submit"}]])

       (if-let [error (get-in asession [:session/error :error/message])]
         [:div {:class "error"} error]
         '())]

      (stats-view asession)

      (if (= 0 (get-in asession [:session/stats :stats/points]))
        ;; first round no points? show the instructions
        (instruction-view)
       '())

      [:div {:class "mtop-50 r"}
       [:form {:action "/giveup" :method "post"}
        [:input {:class "button-primary" :type "submit" :value "Aufgeben"}]]

       [:form {:action "/reset" :method "post"}
        [:input {:class "button-primary" :type "submit" :value "reset"}]]]]))))

(defn give-up-view [asession]
  (html/html
   (doall
    (page
     [:div
      [:center
       (row (mapv vector
                  (repeat :word/miss)
                  (range 1 (inc (count (:session/word asession))))))
       (for [aguess (:session/guesses asession)]
         (row aguess))

       [:form {:class "mtop-50" :action "/reset" :method "post"}
        [:input {:class "button-primary" :type "submit" :value "reset"}]]]

      (stats-view asession)]))))

;; -----------------------------------------------

(defn handle-error
  "an error needs to have a :error/displaycount key with a valid number
  {:session/error {:error/message \"msg\" :error/displaycount 1}}
  "
  [asession]
  (if (:session/error asession)
   (if (< 0 (get-in asession [:session/error :error/displaycount]))
     (update-in asession [:session/error :error/displaycount] dec)
     (dissoc asession :session/error))
   asession))

(defn report-error [asession error-msg]
  (assoc asession :session/error {:error/message error-msg
                                  :error/displaycount 1}))

(defn new-session
  "creates a new game"
  [word]
  {:session/word word
   :session/guesses []
   :session/stats {:stats/points 0
                   :stats/guesses 0
                   :stats/words 0}})

(defn lower-case
  "nil safe: returns an empty string, when s is nil"
  [s]
  (if (nil? s) "" (str/lower-case s)))

(defn handle-stats
  "when we completed a round we want to calculate the points
  and attach the new count to the session"
  [asession]
  (->
   (if (won? asession)
     (let [guesses (count (:session/guesses asession))
           points (points-for-round guesses)]
       (-> asession
           (update-in [:session/stats :stats/points] + points)
           (update-in [:session/stats :stats/words] inc)))
     asession)
   ;; always increase the counter, when we get here
   (update-in [:session/stats :stats/guesses] inc)))

(defn update-session
  "play a round"
  [{session :session params :form-params}]
  (let [user-guess (lower-case (get params "guess"))]
    (if (word-in-list? user-guess)
      (-> session
          (update :session/guesses conj (guess (:session/word session) user-guess))
          (handle-stats))

      (report-error session (str "Das Wort: " user-guess " kenne ich nicht.")))))

(def app
  (ring/ring-handler
    (ring/router
     [["/" {:get (fn [{session :session}]
                   (if (empty? session)
                     ;; first visit or empty session
                     (let [asession (new-session (rand-nth words))]
                       {:status 200
                        :headers {"Content-Type" "text/html"}
                        :body (get-page asession)
                        :session asession})
                     ;; just render the session
                     (let [asession (handle-error session)]
                      {:status 200
                       :headers {"Content-Type" "text/html"}
                       :body (get-page asession)
                       :session asession})))}]

      ["/guess" {:post (fn [request]
                         {:status 303
                          :headers {"Location" "/"}
                          :session (update-session request)})}]

      ["/inspect" {:get (fn [{session :session}]
                          {:status 200
                           :body (str session)})}]

      ["/giveup" {:post (fn [{asession :session}]
                          (if (won? asession)
                            {:status 303
                             :headers {"Location" "/"}}
                            {:status 200
                             :headers {"Content-Type" "text/html"}
                             :body (get-page
                                    (update asession :session/guesses conj (guess (:session/word asession)
                                                                                  (:session/word asession))))
                             :session (-> asession
                                          (assoc :session/word "revelio")
                                          (assoc :session/guesses []))}))}]

      ["/reset" {:post (fn [{asession :session}]
                        {:status 303
                         :headers {"Location" "/"}
                         :session (-> asession
                                      (assoc :session/word (rand-nth words))
                                      (assoc :session/guesses []))})}]]

      {:data {:muuntaja m/instance
      	      :middleware [params/wrap-params
                           muuntaja/format-middleware
                           coercion/coerce-exceptions-middleware
                           coercion/coerce-request-middleware
                           coercion/coerce-response-middleware]}})
    (ring/routes
     (ring/create-resource-handler {:path "/"})
     (ring/create-default-handler))
    {:middleware [session/wrap-session]}))

(defn- add-gzip-handler [server]
  (.setHandler server
               (doto (GzipHandler.)
                 (.setIncludedMimeTypes (into-array ["text/css"
                                                     "text/html"
                                                     "text/plain"]))
                 (.setMinGzipSize 1024)
                 (.setHandler (.getHandler server)))))

(defn -main []
  (jetty/run-jetty #'app {:port 3000
                          :join? false
                          :configurator add-gzip-handler}))

(comment

  (def server (jetty/run-jetty #'app {:port 3000 :join? false
                                      :configurator add-gzip-handler
                                      }))

  (.stop server)

  (def aguess (mapv vector (diff-word "banana" "baneny") (seq "banana")))

  (def asession
    {:session/word "banane"
     :session/guesses [[[:word/match \b]
                        [:word/match \a]
                        [:word/match \n]
                        [:word/miss \a]
                        [:word/match \n]
                        [:word/miss \a]]
                       [[:word/match \b]
                        [:word/match \a]
                        [:word/match \n]
                        [:word/miss \a]
                        [:word/match \n]
                        [:word/miss \a]]
                       [[:word/match \b]
                        [:word/match \a]
                        [:word/match \n]
                        [:word/miss \a]
                        [:word/match \n]
                        [:word/miss \e]]]
     :session/stats {:stats/points 0
                     :stats/guesses 0
                     :stats/words 0}})

  *1)
