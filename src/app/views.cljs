(ns app.views
  (:require [oz.core :as oz]
            [reagent.core :as r]
            [goog.string :as gstring]
            [app.model.core :as m]
            [app.model.protocol :as p]))

(defonce app-state
  (r/atom {}))

(defn init []
  (reset! app-state {:model (m/get-model :seir)}))


(defn line-plot [state]
  (let [values (m/->plot-values (:model state))]
    {:layer [
               {:data {:values values}
                :width 600
                :height 500
                :encoding {:x {:field "x"
                               :axis {:title "Days"}}
                           :y {:field "y"
                               :stack true
                               :axis {:title "People"}}
                           :color {:field "col" :type "nominal"
                                   :scale {:scheme ["yellow" "red" "green" "blue"]}
                                   :legend {:title "Legende"}}
                           :order {:field "order" :type "nominal"}}
                :mark "area"}]}))

(defn header
  []
  [:div
   [:h1 "SEIR Model"]])

(defn set-fn [key]
  (fn [event]
    (swap! app-state assoc-in [:model key] (js/parseFloat (.-value (.-target event))))))

(defn variable-slider [state {:keys [key min max step]}]
  [:tr [:td [:label {:for (str (name key) "-slider")} (str (name key) "=" (key (:model state)))]]
   [:td [:input {:type "range" :min min :max max :value (key (:model state)) :step step :class "slider" :id (str (name key) "-slider")
                 :on-input (set-fn key)}]]])

(defn app []
  (p/variables (:model @app-state))
  [:div
   [header]
   [oz.core/vega-lite (line-plot @app-state)]
   [:div 
    (into [:table] (mapv (partial variable-slider @app-state) (p/variables (:model @app-state))))]
   [:div (gstring/format "R0=T_r/T_c=%.2f" (/ (:T_r (:model @app-state)) (:T_c (:model @app-state))))]])

