(ns shakespeare-game.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [shakespeare-game.helpers :refer
    [jsonp-get elt get-attr set-attr
     add-event-listener remove-node remove-attr
     string-to-dom dom-to-string inner-html]]
   [clojure.string :as string]
   [cljs-http.client :as http]
   [cljs.core.async :refer [<! chan put! sliding-buffer]]))

(enable-console-print!)

(defonce app-state (atom {:selected-play "http://shakespeare.mit.edu/hamlet/full.html"}))

(defn normalize [w] (-> w (string/replace #"\W", "") string/lower-case))

(defn blankify [w]
  (let [underscores  (string/replace w #"\w", "_")]
    underscores))

(defn nodelist-to-seq
  [nl]
  (let [result-seq (map #(.item nl %) (range (.-length nl)))]
    (doall result-seq)))


(defn hintify [s]
  (->> s
       (partition-by (fn [c] (= c "_")))
       (map (fn [snippet]  (if (= "_" (first snippet))
                            (count snippet)
                            (apply str snippet))))
       (apply str)))

(defn split-words [s]
  (let [s (string/replace s #"--" "-- ")
        words (string/split s #"\s+")
        words (remove empty? words)]
    (map (fn [w]
           (let [normalized (normalize w)
                 blanked (blankify w)
                 hinted (hintify blanked)]
             {
              :raw w
              :match-key normalized
              :hint hinted}))
         words)))

(def script (elt "root"))
(def guess-box (elt "guess-box"))

(defn reveal [$w]
  (let [reveal-text (get-attr $w "data-reveal")]
    (inner-html $w reveal-text)
    (remove-attr $w "data-guess")
    (remove-attr $w "style")
    (set-attr $w "class" "guessed last-guessed")))

(defn mark-old [$w]
    (set-attr $w "class" "guessed"))

(defn render-score []
  (let [$delta (elt "delta")
        $score (elt "score")]
    (inner-html $delta (:delta @app-state))
    (inner-html $score (:score @app-state))))

(defn $
  ([s] ($ s js/document))
  ([s doc]
     (-> (.querySelectorAll doc s)
         nodelist-to-seq)))

(defn on-guess [e]
  (let [guesses (split-words (.-value guess-box))
        query-paths (map  #(str "span[data-guess='" (:match-key %) "']") guesses)
        revealed-words (mapcat $ query-paths)]
    (when-not (empty? guesses)
      (set! guess-box.value "")
      (swap! app-state update-in [:score] #(+ % (count revealed-words)))
      (swap! app-state assoc-in [:delta] (count revealed-words))
      (let [last-words (@app-state :last-words)]
        (doall (map mark-old last-words)))
      (swap! app-state assoc-in [:last-words] revealed-words)
      (render-score)
      (->> revealed-words (map reveal) doall))))

(defonce listeners (atom #{}))

(defn listen-to-guess []
  (doall (map #(.removeEventListener guess-box "change" %) @listeners))
  (add-event-listener guess-box "change" on-guess)
  (reset! listeners #{on-guess}))

(defn render-word [w]
  (str "<span class='to-guess' data-guess='" (:match-key w)
       "' data-reveal='" (string/replace (:raw w) #"'" "&#39;")
       "' style='width: " (/ (count (:raw w)) 2)  "em'>"
       ; "<span class='hint'>" (:hint w) "</span>"
       (:hint w) "</span>"))

(defn tag-words [$node]
  (let [contents (.-textContent $node)
        replacement (->>
                     contents
                     split-words
                     (map render-word)
                     (string/join " "))]
    (inner-html $node replacement)))

(defn select-and-apply
 [body-dom f patterns]
 (doseq [selector patterns
         $node ($ selector body-dom)] (f $node)))

(def dom-processing-steps
  {remove-node #{"link" "table"} tag-words #{"blockquote a"}})

(defn render [body-dom]
  (doseq [[f pat] dom-processing-steps]
    (select-and-apply body-dom f pat)))

(defn get-requested-play []
  (let [hash (-> js/window.location.hash 
                 string/trim
                 (subs 1))
        play-slug (re-matches #"^[^\.]+$" hash)
        scene-slug (re-matches #"^([^\.]+).\d.\d$" hash)
        url (cond
             play-slug (str "shakespeare.mit.edu/"
                            play-slug "/full.html")
             scene-slug (str "shakespeare.mit.edu/"
                             (nth scene-slug 1) "/"
                             (nth scene-slug 0) ".html")
             :else "shakespeare.mit.edu/hamlet/hamlet.3.1.html")]
    (swap! app-state assoc-in [:selected-play] url)))

(defn on-init []
  (println "loading with " (:selected-play @app-state))
  (inner-html script "loading...")
  (listen-to-guess)
  (get-requested-play)
  (go (let [response (<! (http/get (@app-state :selected-play)))
            body (:body response)
            body-dom (string-to-dom body)]
        (reset! app-state {:score 0 :delta 0 :last-words []})
        (render body-dom)
        (render-score)
        (inner-html script body-dom))))

(add-event-listener js/window "hashchange" on-init)
(add-event-listener js/window "beforeunload"
  (fn [e] (let [msg "Progress will be lost."
                e (or e js/event)]
            (when e (aset e "returnValue" msg))
            msg)))

(defonce start-once (on-init))
