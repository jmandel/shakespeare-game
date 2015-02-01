(ns shakespeare-game.helpers
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [clojure.string :as string]
   [cljs-http.client :as http]
   [cljs.core.async :refer [<! chan put! sliding-buffer]]))

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
