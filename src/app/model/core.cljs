(ns app.model.core
  (:require [app.model.protocol :as p]
            [app.model.seir :as seir]
            [app.model.seikr :as seikr]))

(defn get-model [kw]
  (case kw
    :seir (seir/init)
    :seikr (seikr/init)))

(defn ->plot-values* [model]
  (reduce into (map-indexed (fn [index model] (p/->plot-point model index)) (take 600 #_#(< 21000 (+ (:I %) (:E %)))
                                                                              (iterate p/step (p/step model))))))

(def ->plot-values (memoize ->plot-values*))