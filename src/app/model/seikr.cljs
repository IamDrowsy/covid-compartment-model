(ns app.model.seikr
  (:require [app.model.protocol :as p]))

;Total Number
(def N 500000)
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

; Medcare capacity
(def KC 2000)
; ICU capacity
(def XC 200)

(def p_I->X (* p_I->K p_K->X))
(def p_R->D (* p_I->X p_X->D))

(defn step [{:keys [S E IKX RD K>KC R>KC X>XC R>XC T_c T_r KC XC] :as state}]
  (let [β (/ 1.0 T_c)
        γ (/ 1.0 T_r)
        dS (- (* β IKX S (/ 1 N)))
        dE (- 0 dS (* a E))
        dR>KC (* γ K>KC)
        dR>XC (* γ X>XC)
        dRD (- (* γ IKX) dR>KC dR>XC)
        RD (+ RD dRD)
        dIKX (- (* a E) (* γ IKX))
        IKX (+ IKX dIKX)
        K (* (- p_I->K p_I->X) IKX)
        X (* p_I->X IKX)]
    (merge state {:S (+ S dS) :dS dS
                  :E (+ E dE) :dE dE
                  :IKX IKX :dI dIKX
                  :I (* (- 1 p_I->K) IKX)
                  :K<KC (min K KC)
                  :K>KC (max (- K KC) 0)
                  :X<XC (min X XC)
                  :X>XC (max (- X XC) 0)
                  :RD RD :dRD dRD
                  :D (* p_R->D RD)
                  :R (* (- 1 p_R->D) RD)
                  :R>KC (+ R>KC dR>KC)
                  :R>XC (+ R>XC dR>XC)
                  :KC KC
                  :KXC (+ XC KC)
                  :XC XC})))

(defn ->plot-point [{:keys [S E I K<KC K>KC X<XC X>XC D R R>KC R>XC KC XC KXC ]} index]
  (map-indexed (fn [index m] (assoc m :order index))
               (reverse [{:x index :y S :col "S"}
                         {:x index :y E :col "E"}
                         {:x index :y R :col "R"}
                         {:x index :y D :col "D"}
                         {:x index :y R>KC :col "R>KC"}
                         {:x index :y R>XC :col "R>XC"}
                         {:x index :y I :col "I"}
                         {:x index :y K>KC :col "K>KC"}
                         {:x index :y X>XC :col "X>XC"}
                         {:x index :y K<KC :col "K<KC"}
                         {:x index :y X<XC :col "X<XC"}
                         {:x index :KC KC :XC XC :KXC KXC :col "const"}])))

(def initial 
  {:T_c 4
   :T_r 12
   :KC KC
   :XC XC
   :S S_0 :I I_0 :R R_0 :E E_0})

(defn variables [_]
  [{:key :T_c :min 1 :max 20 :step 0.1}
   {:key :T_r :min 10 :max 15 :step 0.5}])

(defn colors [_] {:S "blue"
                  :E "blue"
                  :I "#fdae6b"
                  :K<KC "orange"
                  :K>KC "darkorange"
                  :X<XC "red"
                  :X>XC "darkred"
                  :R>KC "violet"
                  :R>XC "purple"
                  :R "green"
                  :D "black"})

(defn init []
  (with-meta initial
    {`p/step step
     `p/->plot-point ->plot-point
     `p/variables variables
     `p/colors colors}))