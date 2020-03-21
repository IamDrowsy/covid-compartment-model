(ns app.model.seikr
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
; Probability to need med care
(def p_I->K 0.045)
; Probability to need ICU if in med care
(def p_K->X 0.25)
; Probability to die if in ICU
(def p_X->D 0.5)

(def p_I->X (* p_I->K p_K->X))
(def p_R->D (* p_I->X p_X->D))

(defn step [{:keys [S E IKX I K X RD R D T_c T_r] :as state}]
  (let [β (/ 1.0 T_c)
        γ (/ 1.0 T_r)
        dS (- (* β IKX S (/ 1 N)))
        dE (- 0 dS (* a E))
        dRD (* γ IKX)
        RD (+ RD dRD)
        dIKX (- (* a E) (* γ IKX))
        IKX (+ IKX dIKX)]
    (merge state {:S (+ S dS) :dS dS
                  :E (+ E dE) :dE dE
                  :IKX IKX :dI dIKX
                  :I (* (- 1 p_I->K) IKX)
                  :K (* p_I->K IKX)
                  :X (* p_I->X IKX)
                  :RD RD :dRD dRD
                  :D (* p_R->D RD)
                  :R (* (- 1 p_R->D) RD)})))

(defn ->plot-point [{:keys [S E I K X D R]} index]
  [{:x index :y E :col "E" :order 0}
   {:x index :y I :col "I" :order 1}
   {:x index :y K :col "K" :order 2}
   {:x index :y X :col "X" :order 3}
   {:x index :y D :col "D" :order 4}
   {:x index :y R :col "R" :order 5}
   {:x index :y S :col "S" :order 6}])

(def initial 
  {:T_c 4
   :T_r 12
   :steps 300
   :S S_0 :I I_0 :R R_0 :E E_0})

(defn variables [_]
  [{:key :T_c :min 1 :max 20 :step 0.1}
   {:key :T_r :min 10 :max 15 :step 0.5}
   {:key :steps :min 20 :max 600 :step 1}])

(defn colors [_] {:S "blue"
                  :E "yellow"
                  :I "#fdae6b"
                  :K "orange"
                  :X "red"
                  :R "green"
                  :D "black"})

(defn init []
  (with-meta initial
    {`p/step step
     `p/->plot-point ->plot-point
     `p/variables variables
     `p/colors colors}))