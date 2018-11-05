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


(defn gen-barabasi-albert
  "Generate a preferential attachment graph as described in Barabasi
  and Albert (1999)."
  ([g num-nodes num-edges seed]
   (let [rnd (java.util.Random. seed)
         ;; initialize graph with two connected nodes
         ;; with equal probability, a new node will attach to
         ;; either one
         g-0 (g/add-edges g [0 1])
         ;; predicate for deciding wether a node
         ;; should be connected to a new node
         connect? (fn [g node]
                    (let [degree-node (count (g/successors g node))
                          degree-sum (reduce #(+ %1 (count (g/successors g %2))) 0 (g/nodes g))]
                      (<= (/ degree-node degree-sum) (.nextDouble rnd))))
         ;; go through all nodes in g and decide whether
         ;; they connect to new
         new-edges (fn [g new]
                     (for [n (g/nodes g)
                           :when (connect? g n)]
                       [new n]))
         ;; compute num-edges edges for new in graph g
         get-new-edges-and-connect (fn [g new num-edges]
                                     (as-> g v
                                       (new-edges v new)
                                       (take num-edges v)
                                       (filter #(= 2 (count %)) v)
                                       (apply g/add-edges g v)))
         ;; two nodes are already in the initialized graph
         ;; the remaining notes will be added
         remaining-nodes (range 2 num-nodes)
         ]

     (reduce #(get-new-edges-and-connect %1 %2 num-edges) g-0 remaining-nodes)))
  ([g num-nodes num-edges]
   (gen-barabasi-albert g num-nodes num-edges (System/nanoTime))))


