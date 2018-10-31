(ns diffusion.gen
  (:require [loom.graph :as g]
            [loom.attr :as a]))


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


(defn create-graph-from-adj-list
  [adj-lst]
  (-> adj-lst
      (g/graph)
      (a/add-attr-to-all :green? 0)
      (a/add-attr-to-all :time-without 0)))


;; "deprecated",
(defn create-graph-from-snap-edge-list
  "Creates a graph from an edge list as created by one of the functions from the
  Stanford Network Analysis Project (SNAP),"
  [path]
  (as-> path v
    (slurp v)
    (re-seq #"\d+" v)
    (map #(Integer/parseInt %) v)
    (partition 2 v)
    (apply g/graph v)))


;(def path "datasets/data")
(def path "/home/dusty/dev/python/snappy/edgeList.txt")

