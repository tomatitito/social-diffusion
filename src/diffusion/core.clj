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


(defn gen-circle
  "Adds num-nodes nodes to graph g and connects each one to out-degree
  other nodes."
  [g num-nodes out-degree]
  {:pre [(> num-nodes (* out-degree 2))]}
  (let [nodes (range num-nodes)
        edges (for [n nodes
                    d (range 1 (inc out-degree))]
                [n (mod (+ n d) (count nodes))])]
    (-> g
        (g/add-nodes* nodes)
        (g/add-edges* edges))))


(defn ^:private add-shortcuts
  "Computes additional edges for graph g as described in Newman and Watts (1999)."
  ([g phi seed]
   (let [rnd (java.util.Random. seed)
         nodes (g/nodes g)
         shortcuts (for [n nodes
                         :when (> phi (.nextDouble rnd))]
                     [n (.nextInt rnd (count nodes))])]
     (-> g
         (g/add-edges* shortcuts)))))


(defn gen-newman-watts
  "Generate a graph with small-world properties as described in Newman and Watts
  (1999)."
  ([g num-nodes out-degree phi seed]
   (-> g
       (gen-circle num-nodes out-degree)
       (add-shortcuts phi seed)))
  ([g num-nodes out-degree phi]
   (gen-newman-watts g num-nodes out-degree phi (System/nanoTime))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(defn create-graph-from-adj-list
  [adj-lst]
  (-> adj-lst
      (g/graph)
      (a/add-attr-to-all :green? 0)
      (a/add-attr-to-all :time-without 0)))


;(def path "datasets/data")
(def path "/home/dusty/dev/python/snappy/edgeList.txt")

(defn create-graph-from-snap-edge-list
  [path]
  (as-> path v
        (slurp v)
        (re-seq #"\d+" v)
        (map #(Integer/parseInt %) v)
        (partition 2 v)
        (apply g/graph v)))


(def adj-list {0 [1 4] 1 [0 2 3] 2 [1 3 4] 3 [1 2] 4 [0 2 5 6] 5 [4 6] 6 [4 5]})


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