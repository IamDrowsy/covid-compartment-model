(ns app.views
  (:require [oz.core :as oz]
            [reagent.core :as r]
            [goog.string :as gstring]
            [app.model.core :as m]
            [app.model.protocol :as p]
            [app.conf.colors :refer [compartment->color]]))

(defonce app-state
  (r/atom {:model (m/get-model :seikr)}))

(reset! app-state {:model (m/get-model :seikr)})

(defn bar-chart [app-state]
  (let [T_S   10
        T_max 42
        values [{:severity :normal    :state :S     :days T_S}
                {:severity :normal    :state :E     :days 2.5}
                {:severity :normal    :state :I     :days 10}
                {:severity :normal    :state :R     :days (- T_max T_S 2.5 10)}
                {:severity :hospital  :state :blank :days (+ T_S 9)}
                {:severity :hospital  :state :K     :days 14}
                {:severity :hospital  :state :R     :days (- T_max T_S 9 14)}
                {:severity :intensive :state :blank :days (+ T_S 10)}
                {:severity :intensive :state :X     :days 10}
                {:severity :intensive :state :R     :days (- T_max T_S 10 10)}
                {:severity :lethal    :state :blank :days (+ T_S 20)}
                {:severity :lethal    :state :D     :days (- T_max T_S 10 10)}]
        values+order (map-indexed (fn [i v] (assoc v :order i)) values)]
       {:data {:values values+order}
        :mark "bar"
        :encoding {:y {:field :severity :type "nominal"
                       :sort {:field :order :op :min}}
                   :x {:aggregate "sum" :field :days :type "quantitative"}
                   :order {}
                   :color {:field :state :type "nominal"
                           :scale {:range (map #(get compartment->color % "white") [:S :E :I :R
                                                                                    :blank :K :X :D])}
                           :sort {}}}}))


(defn line-plot [state]
  (let [values (m/->plot-values (:model state))
        consts (filter #(= (:col %) "const") values)]
    {:layer [{:data {:values values}
              :transform [{:filter {:not {:field "col" :oneOf ["const"]}}}]
              :width 600
              :height 500
              :encoding {:x {:field "x"
                             :type "quantitative"
                             :axis {:title "Days"}}
                         :y {:field "y"
                             :type "quantitative"
                             :stack true
                             :scale {:domain [0 10000]}
                             :axis {:title "People"}}
                         :color {:field "col" :type "nominal"
                                 :scale {:range (vals (sort-by key (p/colors (:model state))))}
                                 :legend {:title "Legende"}}
                         :order {:field "order" :type "ordinal"}}
              :mark {:type "area"
                     :clip true}}
             {:data {:values consts}
              :width 600
              :heigh 500
              :encoding {:x {:field "x"
                             :type "quantitative"}
                         :y {:field "KXC"
                             :type "quantitative"}}
              :mark "line"}
             {:data {:values consts}
              :width 600
              :heigh 500
              :encoding {:x {:field "x"
                             :type "quantitative"}
                         :y {:field "XC"
                             :type "quantitative"}}
              :mark "line"}]}))

(defn header
  []
  [:div
   [:h1 "SEIR Model"]])

(defn sources
  []
  [:div [:h3 "Sources"]
    [:ul [:li [:a {:href "https://medium.com/@tomaspueyo/coronavirus-act-today-or-people-will-die-f4d3d9cd99ca"} "coronavirus-act-today-or-people-will-die"]]]
    [:ul [:li [:a {:href "https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Modellierung_Deutschland.pdf?__blob=publicationFile"} "RKI Modellierung von Beispielszenarien der SARS-CoV-2-Epidemie 2020"]]]
    [:ul [:li [:a {:href "https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology#The_SEIR_model"} "SEIR model"]]]
    [:ul [:li [:a {:href "https://web.br.de/interaktiv/corona-simulation/"} "corona-simulation"]]]
    [:ul [:li [:a {:href "https://www.ndr.de/nachrichten/info/podcast4684.html"} "Das Coronavirus-Update mit Christian Drosten"]]]])

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
   [oz.core/vega-lite (bar-chart @app-state)]
   [oz.core/vega-lite (line-plot @app-state)]
   [:div 
    (into [:table] (mapv (partial variable-slider @app-state) (p/variables (:model @app-state))))]
   [:div (gstring/format "R0=T_r/T_c=%.2f" (/ (:T_r (:model @app-state)) (:T_c (:model @app-state))))]
   (sources)])

