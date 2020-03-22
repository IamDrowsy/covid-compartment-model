(ns app.model.seikr
  (:require [app.model.protocol :as p]
            [app.conf.labels :refer [label]]))

;Total Population Number
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

; Medcare capacity
(def KC 2000)
; ICU capacity
(def XC 200)

(defn step [{:keys [T_c T_r p_I->K p_K->X p_X->D p_I->X p_R->D KC XC
                    S E IKX RD K>KC R>KC X>XC R>XC ] :as state}]
  (let [β (/ 1.0 T_c)
        γ (/ 1.0 T_r)
        p_I->X (or p_I->X (* p_I->K p_K->X))
        p_R->D (or p_R->D (* p_I->X p_X->D))
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

(defn keys-for-tooltip [all]
  (reduce (fn [result k]
            (let [num (Math/round (all k 0))]
              (if (label k)
                (assoc result (label k)
                       num)
                result)))
          {}
          (keys all)))

(defn ->plot-point [{:keys [S E I K<KC K>KC X<XC X>XC D R R>KC R>XC KC XC KXC ] :as all} index]
  (map-indexed (fn [index m] (merge (keys-for-tooltip all) (assoc m :order index :label (label (:key m) "const"))))
               (reverse [{:x index :y S :key :S}
                         {:x index :y E :key :E}
                         {:x index :y R :key :R}
                         {:x index :y R>KC :key :R>KC}
                         {:x index :y R>XC :key :R>XC}
                         {:x index :y D :key :D}
                         {:x index :y I :key :I}
                         {:x index :y K>KC :key :K>KC}
                         {:x index :y X>XC :key :X>XC}
                         {:x index :y K<KC :key :K<KC}
                         {:x index :y X<XC :key :X<XC}
                         {:x index :KC KC :XC XC :KXC KXC}])))

(def initial 
  {:T_c 4
   :T_r 12
   :KC KC
   :XC XC
   :p_I->K 0.045
   :p_K->X 0.25
   :p_X->D 0.5
   :S S_0 :I I_0 :R R_0 :E E_0})

(defn variables [_]
  [{:key :T_c :min 1 :max 8 :step 0.5}
   #_{:key :T_r :min 10 :max 15 :step 0.5}])

(defn init []
  (with-meta initial
    {`p/step step
     `p/->plot-point ->plot-point
     `p/variables variables}))
