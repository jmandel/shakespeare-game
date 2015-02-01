(ns shakespeare-game.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
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
  (let [words (string/split s #"\s+")
        words (remove empty? words)]
    (map (fn [w]
           (let [normalized (normalize w)
                 blanked (blankify w)
                 hinted (hintify blanked)]
             {
              :raw w
              :match-key normalized
              :blank blanked
              :hint hinted}))
         words)))

(defn elt [id] (.getElementById js/document id))
(defn get-attr [$e a] (.getAttribute $e a))
(defn set-attr [$e a val] (.setAttribute $e a val))
(defn add-event-listener [$n e l] (.addEventListener $n e l))
(defn remove-attr [$e a] (.removeAttribute $e a))
(defn remove-node [$node] (-> $node 
                              (.-parentNode)
                              (.removeChild $node)))
(defn string-to-dom [s]
  (let [parser (js/DOMParser.)]
    (.parseFromString parser s "text/html")))

(defn dom-to-string [$n]
  (let [serializer (js/XMLSerializer.)]
    (.serializeToString serializer $n)))

(defn inner-html [$e val]
  (let [strval (if (= js/HTMLDocument (type val))
                (dom-to-string val)
                 val)]
    (set! (.-innerHTML $e) strval)))

(def script (elt "root"))
(def guess-box (elt "guess-box"))

(defn reveal [$w]
  (let [reveal-text (get-attr $w "data-reveal")]
    (inner-html $w reveal-text)
    (remove-attr $w "data-guess")
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
  (let [the-guess (-> (.-value guess-box) normalize)
        query-path (str "span[data-guess='" the-guess "']")
        revealed-words ($ query-path)]
    (when the-guess
      (set! (.-value guess-box) "")
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

(defn tag-words-in [$node]
  (let [contents (.-textContent $node)
        replacement (->>
                     contents
                     split-words
                     (map render-word)
                     (string/join " "))]
    (inner-html $node replacement)))

(def dialog-css-patterns #{"blockquote a"})
(def delete-css-patterns #{"link" "table"})

(defn render [body-dom]
  (set! (.-a js/window) body-dom)
  (doall (for [selector delete-css-patterns
               $node ($ selector body-dom)]
           (remove-node $node)))
  (doall (for [selector dialog-css-patterns
               $node ($ selector body-dom)]
           (tag-words-in $node))))

(defn random-char [] (rand-nth "abcdefghijklmnopqrstuvwxyz"))
(defn random-word [n] (apply str (take n (repeatedly random-char))))

(defonce any-url "http://www.whateverorigin.org/get?callback=")
(defn jsonp-get [url]
  (let [c (chan)
        cb-name (random-word 36)
        jsonp-url (str any-url cb-name "&url=" url)
        script-node (doto (.createElement js/document "script")
                      (aset "async" true)
                      (aset "src" jsonp-url)
                      (aset "id" cb-name))
        cb (fn [response]
             (js-delete js/window cb-name)
             (remove-node script-node)
             (put! c response))]
    (let [head (-> (.getElementsByTagName js/document "head")
                   (aget 0))]
      (.appendChild head script-node))
    (aset js/window cb-name cb)
    c)) 

(defn get-requested-play []
  (let [hash (-> js/window.location.hash 
                 string/trim
                 (subs 1)) 
        url (cond
         (re-matches #"^http://.*" hash) hash
         (re-matches #"^[a-z]+$" hash) (str "http://shakespeare.mit.edu/" hash "/full.html")
         :else "http://shakespeare.mit.edu/hamlet/full.html")]
    (swap! app-state assoc-in [:selected-play] url)))


(defn on-init []
  (println "loading with " (:selected-play @app-state))
  (inner-html script "loading...")
  (listen-to-guess)
  (get-requested-play)
  (go (let [response (<! (jsonp-get (@app-state :selected-play)))
            body (aget response "contents")
            body-dom (string-to-dom body)]
        (reset! app-state {:score 0 :delta 0 :last-words []})
        (render body-dom)
        (render-score)
        (inner-html script body-dom))))

(add-event-listener js/window "hashchange" on-init)
(defonce start-once (on-init))
