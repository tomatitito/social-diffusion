(ns diffusion.core
  (:require [loom.graph :as g]
            [loom.attr :as a]
            [loom.io :as io]
            [loom.gen :as gen]
            [oz.core :as oz]
            [ministrants.core :as m]
            [anglican.core :refer [doquery]]
            [diffusion.model :refer [diffusion-query initialize-graph]]
            [diffusion.io :as dio])
  (:gen-class))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


;(def g
;  (-> path
;      (create-graph-from-snap-edge-list )
;      (initialize-graph 0.01)))
;
;(def g2
;  (->
;    (gen-newman-watts (g/graph) 1000 10 0.3)
;    (initialize-graph 0.1)))
;
(def g3
  (->
    (gen-newman-watts (g/graph) 10 2 0.1)
    (initialize-graph 0.1)))

(def gn (gen-newman-watts (g/graph) 100 3 0.3))
(io/dot gn "testgraph.dot")

(def samples (doall (take 1000 (doquery :smc diffusion-query [g3] :number-of-particles 10))))
;(first samples)
;(count (g/nodes g))

(dio/write-seasons! samples #(m/from-result % [:history :n-green]) "ngreen2.csv")