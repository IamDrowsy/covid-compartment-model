(ns app.model.seir
  (:require [app.model.protocol :as p]))

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

(defn ->plot-point [{:keys [S E I R]} index]
  [{:x index :y S :col "S" :order 3}
   {:x index :y E :col "E" :order 0}
   {:x index :y I :col "I" :order 1}
   {:x index :y R :col "R" :order 2}])

(def initial 
  {:T_c 3.6
   :T_r 12
   :steps 300
   :S S_0 :I I_0 :R R_0 :E E_0})

(defn variables [_]
  [{:key :T_c :min 1 :max 20 :step 0.1}
   {:key :T_r :min 10 :max 15 :step 0.5}
   {:key :steps :min 20 :max 600 :step 1}])

(defn colors [_]
  {:S "blue"
   :E "yellow"
   :I "red"
   :R "green"})

(defn init []
  (with-meta initial
    {`p/step step
     `p/->plot-point ->plot-point
     `p/variables variables
     `p/colors colors}))