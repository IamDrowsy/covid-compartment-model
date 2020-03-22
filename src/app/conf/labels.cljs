(ns app.conf.labels)

(def label {:S "Anfällig"
            :E "Angesteckt"
            :I "Infektiös"
            :R "Geheilt"
            :K "Krankenhaus"
            :X "Intensivstation"
            :D "Verstorben"
            :K<KC "Krankenhaus"
            :K>KC "Benötigt Krankenhaus, aber überlastet"
            :X<XC "Intensivstation"
            :X>XC "Benötigt Intensivstation, aber überlasted"
            :R>KC "Benötigte Krankenhaus, Zustand unklar"
            :R>XC "Benötigte Intensivstation, Zustand unklar"})