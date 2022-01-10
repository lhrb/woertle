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
  (:gen-class))

(def words
 (with-open [reader (io/reader "resources/words.txt")]
   (doall (line-seq reader))))

(defn won? [asession]
  (->> (last (:session/guesses asession))
       (map second)
       (apply str)
       (= (:session/word asession))))

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

(defn get-page [asession]
  (html/html
   (doall
    (page
     [:div
      [:center
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
          [:input {:class "button-primary" :type "submit" :value "submit"}]])]


      [:div {:class "info-box"}
       [:h3 "Anleitung"]
       [:p "versuche das Wort zu erraten. Dabei helfen dir die Farben:"]
       [:word2 [:letter-box {:class "match"} [:letter "a"]][:p "Der Buchstabe kommt im Wort vor und ist an der richtigen Stelle."]]
       [:word2 [:letter-box {:class "contains"} [:letter "a"]][:p "Der Buchstabe kommt im Wort vor, steht aber an der falsche Stelle."]]
       [:word2 [:letter-box {:class "miss"} [:letter "a"]][:p "Der Buchstabe kommt leider gar nicht vor"]]
       [:br]
       [:p "Fun fact: Es gibt insgesamt 12435 Wörter und alle Wörter kommen aus den Harry Potter Büchern und ja 'aargh' und 'aarrgh' sind natürlich gültige Worte!"]]

      [:div {:class "mtop-50 r"}
       [:form {:action "/giveup" :method "post"}
        [:input {:class "button-primary" :type "submit" :value "Aufgeben"}]]

       [:form {:action "/reset" :method "post"}
        [:input {:class "button-primary" :type "submit" :value "reset"}]]]]))))

;; -----------------------------------------------

(defn compare-letter-position
  "compares the letter at a specific position"
  [letters original guess]
  (cond
    (= original guess)  :word/match
    (letters guess)     :word/contains
    :else               :word/miss))

(defn diff-word
  [original guess]
  (let [comp (partial compare-letter-position (set original))]
    (mapv comp (seq original) (seq guess))))

(defn guess [word guess]
  (mapv vector (diff-word word guess) (seq guess)))

(defn new-session
  "creates a new game"
  [word]
  (let [first-row (mapv vector
                        (repeat :word/miss)
                        (range 1 (inc (count word))))]
    {:session/word word
     :session/guesses [first-row]}))

(defn update-session
  "play a round"
  [{session :session params :form-params}]
  (let [user-guess (str/lower-case (get params "guess"))
        guess (guess (:session/word session) user-guess)]
    (update session :session/guesses conj guess)))

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
                     {:status 200
                      :headers {"Content-Type" "text/html"}
                      :body (get-page session)}))}]

      ["/guess" {:post (fn [request]
                         {:status 303
                          :headers {"Location" "/"}
                          :session (update-session request)})}]

      ["/inspect" {:get (fn [{session :session}]
                          {:status 200
                           :body (str session)})}]

      ["/giveup" {:post (fn [request]
                          (let [asession (:session request)
                                solution {"guess" (:session/word asession)}]
                            {:status 303
                             :headers {"Location" "/"}
                             :session (update-session (assoc request :form-params solution))}))}]

      ["/reset" {:post (fn [_]
                        {:status 303
                         :headers {"Location" "/"}
                         :session (new-session (rand-nth words))})}]]

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

(defn -main []
  (jetty/run-jetty #'app {:port 3000, :join? false}))

(comment

  (def server (jetty/run-jetty #'app {:port 3000, :join? false}))

  (.stop server)

  (def aguess (mapv vector (diff-word "banana" "baneny") (seq "banana")))

  (def asession
    {:session/word "banane"
     :session/guesses [[[:word/miss 1]
                        [:word/miss  2]
                        [:word/miss 3]
                        [:word/miss 4]
                        [:word/miss 5]
                        [:word/miss 6]]
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
                        [:word/miss \a]]]})

  *1)
