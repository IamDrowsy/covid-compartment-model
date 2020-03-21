(ns app.conf.labels)

(def label {:S "Anfällig"
            :E "Angesteckt"
            :I "Infektiös"
            :R "Geheilt"
            :K "Krankenhaus"
            :X "Intensivstation"
            :D "Verstorben"
            :K<KC "Krankenhaus"
            :K>KC "Benötigt Krankenhause, aber überlastet"
            :X<XC "Intensivestation"
            :X>XC "Benötigt Intensivestation, aber überlasted"
            :R>KC "Benötigte Krankenhause, Zustand unklar"
            :R>XC "Benötigte Intensivstation, Zustand unklar"})