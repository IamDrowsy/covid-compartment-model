(ns app.views
  (:require [oz.core :as oz]
            [reagent.core :as r]
            [goog.string :as gstring]
            ["calculess" :as c]))


(defn group-data [& names]
  (apply concat (for [n names]
                  (map-indexed (fn [i y] {:x i :y y :col n}) (take 5
                                                                   (repeatedly #(rand-int 100)))))))

(def C (.-prototype c))

;Total Number
(def N 500000.0)
; Number of Exposed
(def E_0 1)
; Number of Infected 
(def I_0 0)
; Number of Susceptible
(def S_0 (- N I_0))
; Number of immune
(def R_0 0)
; Incubation Period
(def a (/ 1 2.5))

(defn step [{:keys [S E I R T_c T_r] :as state}]
  (let [β (/ 1.0 T_c)
        γ (/ 1.0 T_r)
        dS (- (* β I S (/ 1 N)))
        dE (- 0 dS (* a E))
        dR (* γ I)
        dI (- (* a E) (* γ I))]
    (merge state {:S (+ S dS) :dS dS
                  :E (+ E dE) :dE dE
                  :I (+ I dI) :dI dI
                  :R (+ R dR) :dR dR})))

(defonce app-state
  (r/atom {}))

(defn init []
  (reset! app-state {:T_c 4
                     :T_r 12
                     :steps 30
                     :S S_0 :I I_0 :R R_0 :E E_0}))

(defn ->plot-point [index {:keys [S E I R]}]
  [{:x index :y S :col "S" :order 3}
   {:x index :y E :col "E" :order 0}
   {:x index :y I :col "I" :order 1}
   {:x index :y R :col "R" :order 2}])


(defn line-plot [state]
  (let [values (apply concat (map-indexed ->plot-point (take (:steps state) (iterate step state))))]
    (prn state)
    (prn values)
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
    (swap! app-state assoc key (js/parseFloat (.-value (.-target event))))))

(defn variable-slider [state {:keys [key min max step]}]
  [:tr [:td [:label {:for (str (name key) "-slider")} (str (name key) "=" (key state))]]
   [:td [:input {:type "range" :min min :max max :value (key state) :step step :class "slider" :id (str (name key) "-slider")
                 :on-input (set-fn key)}]]])

(defn app []
  [:div
   [header]
   [oz.core/vega-lite (line-plot @app-state)]
   [:div 
    (into [:table] (mapv (partial variable-slider @app-state) [{:key :T_c :min 1 :max 20 :step 0.1}
                                                               {:key :T_r :min 10 :max 15 :step 0.5}
                                                               {:key :steps :min 20 :max 600 :step 1}]))]
   [:div (gstring/format "R0=T_r/T_c=%.2f" (/ (:T_r @app-state) (:T_c @app-state)))]])

