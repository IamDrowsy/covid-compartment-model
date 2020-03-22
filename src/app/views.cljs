(ns app.views
  (:require [oz.core :as oz]
            [reagent.core :as r]
            [goog.string :as gstring]
            [app.model.core :as m]
            [app.model.protocol :as p]
            [app.conf.colors :refer [compartment->color]]
            [app.conf.labels :refer [label]]))

(defn initial-state []
  (let [m (m/get-model :seikr)]
    {:simple-model (merge m {:p_I->K 0
                             :p_R->D 0.005})
     :model m}))

(defonce app-state
  (r/atom (initial-state)))

(reset! app-state (initial-state))

(defn bar-chart [app-state &[{:keys [whitelist-severity]}]]
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
                {:severity :lethal    :state :D     :days (- T_max T_S 10 10)}]]
       {:data {:values (->> values
                            (map-indexed (fn [i v] (assoc v :order i :label (label (:state v)))))
                            (filter (if (empty? whitelist-severity)
                                        (constantly true)
                                        #((set whitelist-severity) (:severity %)))))}
        :mark "bar"
        :encoding {:y {:field :severity :type "nominal"
                       :sort {:field :order :op :min}}
                   :x {:aggregate "sum" :field :days :type "quantitative"}
                   :order {}
                   :color {:field :label :type "nominal"
                           :scale {:range (map #(get compartment->color % "white") [:S :E :I :R
                                                                                    :blank :K :X :D])}
                           :sort {}}}}))


(defn line-plot [model &[{:keys [y_max visible-keys show-capacity]}]]
  (let [values (m/->plot-values model)
        key->label (zipmap (map :key values) (map :label values))
        consts (if show-capacity (filter #(= (:label %) "const") values) [])]
    {:layer [{:data {:values values}
              :transform [{:filter {:not {:field "label" :oneOf ["const"]}}}]
              :width 600
              :height 500
              :encoding {:x {:field "x"
                             :type "quantitative"
                             :axis {:title "Days"}}
                         :y {:field "y"
                             :type "quantitative"
                             :stack true
                             :scale (if-not y_max {} {:domain [0 y_max]})
                             :axis {:title "People"}}
                         :color {:field "label" :type "nominal"
                                 :scale {:range (vals (sort-by #(key->label (key %)) (select-keys compartment->color (keys key->label))))}
                                 :legend {:title "Legende" :orient :right
                                          :labelLimit 300
                                          :values (map label visible-keys)}}
                         :order {:field "order" :type "ordinal"}
                         :tooltip (mapv (fn [key] {:field (label key) :type "ordinal"}) visible-keys)}
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
    (swap! app-state assoc-in [:model key] (js/parseFloat (.-value (.-target event))))
    (swap! app-state assoc-in [:simple-model key] (js/parseFloat (.-value (.-target event))))))

(defn variable-slider [state {:keys [key min max step]}]
  [:tr [:td [:label {:for (str (name key) "-slider")} (str (name key) "=" (key (:model state)))]]
   [:td [:input {:type "range" :min min :max max :value (key (:model state)) :step step :class "slider" :id (str (name key) "-slider")
                 :on-input (set-fn key)}]]])

(defn app []
  (p/variables (:model @app-state))
  [:div
   [header]
   [:h4 "Üblicher Krankheitsverlauf eines Patienten"]
   [:p [:i "*angenommen er steckt sich am 10. Tag an"]]
   [oz.core/vega-lite (bar-chart @app-state {:whitelist-severity #{:normal}})]
   [:h4 "Verteilung der Krankheitszustände einer Gesellschaft über den Verlauf der Epidemie"]
   [:p [:i "-> Exponentielles Wachstum erklären"]]
   [:p [:i "-> Sättigung (erst) wenn die meisten die Krankheit überstanden und deshalb immun sind"]]
   [oz.core/vega-lite (line-plot (:simple-model @app-state) {:visible-keys [:S :E :I :R :D]})]
   [:p [:i "-> Indem T_c (die durchschnittliche Zeit in Tagen, zwischen zwei Ansteckungen durch einen Infizierten) gesenkt wird, kann die Kurve abgeflacht und der Zeitpunkt an dem das Maximum Erkrankter erreicht wird herausgezögert werden"]]
   [:h4 "Nicht alle Patienten sind schwach symptomatisch"]
   [:p [:i "TODO Wahrscheinlichkeiten ins Diagramm einzeichnen"]]
   [:p [:i "* Etwa 5% der Patienten müssen ins Krankenhaus; davon etwa 25% auf die Intensivstation; davon etwa 50% sterben"]]
   [oz.core/vega-lite (bar-chart @app-state)]
   [:h4 "Das Gesundheitssystem wird an seine Kapazitätsgrenzen kommen"]
   [:p [:i "-> Wenn es nicht gelingt, die Ansteckungsrate hinreichend abzusenken, werden viele Patienten nicht die erforderliche Behandlung bekommen können"]]
   [oz.core/vega-lite (line-plot (:model @app-state) {:y_max 30000
                                                      :visible-keys [:S :E :I :K<KC :K>KC :R>KC :X<XC :X>XC :R>XC :R :D]
                                                      :show-capacity true})]
   [:p [:i "-> Durch verlangsammen der Epidemie kann der Kollaps der Versorgung verhindert werden"]]
   [:p [:i "-> jeder einzelnen von uns ist gefragt…"]]
   [:div (into [:table] (mapv (partial variable-slider @app-state) (p/variables (:model @app-state))))
         [:div (gstring/format "R0=T_r/T_c=%.2f" (/ (:T_r (:model @app-state)) (:T_c (:model @app-state))))]]
   [:h4 "TODO Verzögerung der Epidemie, so dass neue wirksamere Therapien und Impfungen entwickelt werden können"]
   (sources)])

