(ns shakespeare-game.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]])
  (:require
   [shakespeare-game.helpers :refer
    [$ elt get-attr set-attr replace-node
     add-event-listener remove-node remove-attr
     string-to-dom dom-to-string inner-html]]
   [clojure.string :as string]
   [om.core :as om :include-macros true]
   [om.dom :as dom :include-macros true]
   [cljs-http.client :as http]
   [cljs.core.async :refer [<! chan put! sliding-buffer close! ]]))

(enable-console-print!)
(println "loaded")

(defonce app-state (atom {:score 0 :delta 0 :last-words [] :guessed #{}
                          :url-state {:selected-play "hamlet"}}))
(defn normalize [w] (-> w (string/replace #"\W", "") string/lower-case))

(defn blankify [w]
  (let [underscores  (string/replace w #"\w", "_")]
    underscores))

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

(defn reveal [$w]
  (let [reveal-text (get-attr $w "data-reveal")]
    (inner-html $w reveal-text)
    (remove-attr $w "data-guess")
    (remove-attr $w "style")
    (set-attr $w "class" "guessed last-guessed")))

(defn mark-old [$w]
  (set-attr $w "class" "guessed"))

(defn render-word [w data]
  (str "<span class='to-guess' data-guess='" (:match-key w)
       "' data-reveal='" (string/replace (:raw w) #"'" "&#39;")
       "' style='width: " (/ (count (:raw w)) 2)  "em'>"
                                        ; "<span class='hint'>" (:hint w) "</span>"
       (:hint w) "</span>"))

(defn tag-words [$node data]
  (let [contents (.-textContent $node)
        replacement (->>
                     contents
                     split-words
                     (map render-word)
                     (string/join " "))]
    (inner-html $node replacement)))

(defn replace-pre [$node data]
  (let [$div (.createElement js/document "div")
        text (.-innerText $node)
        lines (string/split text #"\n")
        indent-depth (fn [l] (let [m (re-matches #"^(\t+).+$" l)]
                              (if m (count (nth m 1)) 0)))
        process-line (fn [l] (let [depth (indent-depth l)
                                  is-dialog? (= 2 depth)
                                  words (split-words l)
                                  lead-in (repeat depth "&nbsp;&nbsp;&nbsp;")
                                  f (if is-dialog? #(render-word % data) #(str (:raw %) " "))]
                              (concat ["&nbsp;"] lead-in (string/join " " (map f words)))))
        entire (->> lines
                    (map process-line)
                    (map #(apply str (concat ["<div>"] % ["</div>"])))
                    (string/join "\n"))]
    (inner-html $div entire)
    (replace-node $node $div)))

(defn select-and-apply
  [body-dom f patterns data]
  (doseq [selector patterns
          $node ($ selector body-dom)] (f $node data)))

(def dom-processing-steps
  {:shakespeare 
   [remove-node #{"link" "table"}
    tag-words #{"blockquote a"}]

   :plaintext
   [replace-pre #{"pre"}] 
  })


(defn render [play data]
  (let [steps (dom-processing-steps (:type play))]
    (doseq [[f pat] (partition 2 steps)]
      (select-and-apply (:dom play) f pat data)))
  play)

(defn requested-play []
  (let [hash (-> js/window.location.hash 
                 string/trim
                 (subs 1))
        play-slug (re-matches #"^[^\.]+$" hash)
        scene-slug (re-matches #"^([^\.]+).\d.\d$" hash)]
    (cond
     (re-matches #"jurassic_park" hash) {:type :plaintext :url "movies/jurassic_park.html"}
     play-slug {:type :shakespeare :url (str "shakespeare.mit.edu/" play-slug "/full.html")}
     scene-slug {:type :shakespeare :url (str "shakespeare.mit.edu/"
                                              (nth scene-slug 1) "/"
                                              (nth scene-slug 0) ".html")}
     :else {:type :shakespeare :url  "shakespeare.mit.edu/hamlet/hamlet.3.1.html"})))


(defn app-inputs [data owner {:keys [notify]}]
  (println "Call app-inputs")
  (reify
    om/IWillUnmount
    (will-unmount [_]
      (println "inputs to unmount " ))


    om/IRenderState
    (render-state [_  {:keys [current-value]}]
      (dom/div #js {:className "guess"}
               (dom/input #js {
                               :type "text"
                               :className "guess-box"
                               :ref "guess"
                               :value current-value
                               :autoFocus true
                               :onKeyDown
                               (fn [e]
                                 (let [val (.. e -target -value)
                                       key (.-key e)
                                       guesses (split-words val)]
                                   (when (= key "Enter")
                                     (println "normalized " val guesses notify)
                                     (put! notify guesses)
                                     (om/set-state! owner :current-value ""))))
                               
                               :onChange
                               (fn [e]
                                 (let [val (.. e -target -value)
                                       key (.-key e)]
                                   (om/set-state! owner :current-value val))) 
                               })
               "Gain: " (dom/span #js {:id "delta"} (:delta data)) ", "
               "Score: " (dom/span #js {:id "score"} (:score data))))))

(defn handle-guesses [guesses data owner last-words]
  (let [node (om/get-node owner)
        query-paths (map #(str "span[data-guess='" (:match-key %) "']") guesses)
        revealed-words (mapcat $ query-paths)]
      (om/transact! data [:score] #(+ % (count revealed-words)))
      (om/transact! data [:guessed] #(reduce conj % (remove nil? (map :raw guesses)) ))
      (om/update! data [:delta] (count revealed-words))
      (println "Updated score, delta" guesses (count guesses) (count revealed-words)) 
      (doall (map mark-old last-words))
      (->> revealed-words (map reveal) doall)
    revealed-words))

(defonce counter (atom 0))

(defn app-output [data owner {:keys [notify]}]
  (println "Call app-output")
  (reify
    om/IDidMount
    (did-mount [this]
      (go-loop [last-words []]
               (let [guesses (<! notify)]
                 (when guesses
                   (recur (handle-guesses guesses data owner last-words)))))
      (when (:guessed data)
        (swap! app-state assoc-in [:delta] 0)
        (swap! app-state assoc-in [:score] 0)
        (println "adding gussed" (:guessed data))
        (put! notify (split-words (string/join " " (:guessed data)))) 
        (put! notify [])))

    om/IShouldUpdate
    (should-update [_ next-props next-state]
      (let [props (om/get-props owner)
            same (== (props :script) (next-props :script))]
        (println "Test same" (props :script))
        (not same)))

    om/IWillUnmount
    (will-unmount [_]
      )

    om/IRender
    (render [_]
      (let [dom (get-in data [:script :dom])
            rendered (if dom (dom-to-string dom) "")] 
        (om.dom/div #js {:dangerouslySetInnerHTML #js {:__html rendered}} nil)))))

(defonce hashchange
  (let [c (chan)]
    (add-event-listener js/window "hashchange"
                        (fn [e] (put! c (requested-play)))) c))

(defn set-play [data script]
  (println "GET url" script)
  (go 
   (let [response (<! (http/get (:url script)))
         body-dom (string-to-dom (:body response))
         script (assoc script :dom body-dom)
         _ (render script data)]
     (om/update! data [:script] script))))

(defonce notify (chan))

(defn app-root [data owner]
  (println "Call app-root")
  (reify
    om/IWillMount
    (will-mount [_]
      (let [finish (chan)]
        (om/set-state! owner  :finish finish)
        (go-loop []
                 (alt!
                  hashchange ([new-url] (set-play data new-url) (recur))
                  finish :dont-recur)))) 

    om/IWillUnmount
    (will-unmount [_]
      (println "root to unmount " )
      (close! (om/get-state owner :finish)))

    om/IRender
    (render [_]
      (dom/div nil
               (om/build app-inputs data {:opts {:notify notify}})
               (om/build app-output data {:opts {:notify notify}})))))

(defn on-init []
  (println "On init")

  (close! notify)
  (def notify (chan))

  (om/root app-root app-state {:target (. js/document (getElementById "root"))})
  (put! hashchange (requested-play)))

(add-event-listener js/window "beforeunload"
                    (fn [e] (let [msg "Progress will be lost."
                                 e (or e js/event)]
                             (when e (aset e "returnValue" msg))
                             msg)))

(defonce start-once (on-init))
