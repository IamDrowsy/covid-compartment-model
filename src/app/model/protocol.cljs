(ns app.model.protocol)

(defprotocol Model
  :extend-via-metadata true
  (step [this])
  (->plot-point [this index])
  (variables [this])
  (colors [this]))