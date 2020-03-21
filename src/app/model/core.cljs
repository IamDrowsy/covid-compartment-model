(ns app.model.core
  (:require [app.model.protocol :as p]
            [app.model.seir :as seir]
            [app.model.seikr :as seikr]))

(defn get-model [kw]
  (case kw
    :seir (seir/init)
    :seikr (seikr/init)))

(defn ->plot-values [model]
  (apply concat (map-indexed (fn [index model] (p/->plot-point model index)) (take (:steps model) (iterate p/step model)))))