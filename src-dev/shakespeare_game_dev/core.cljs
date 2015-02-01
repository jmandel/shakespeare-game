(ns shakespeare-game-dev.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [figwheel.client :as fw]
   [shakespeare-game.core :refer [on-init]]))


(println "Starting once")
(fw/start {:on-jsload (fn [] (on-init))})
