(ns diffusion.model
  (:require [loom.graph :as g]
            [loom.attr :as a])
  (:use [anglican runtime emit]))


(defn initialize-graph
  "Add attributes :green? and :time-without to nodes of g. Initialize all
  with zero and set :green? of specified proportion of nodes to 1."
  [g proportion]
  (let
    [n-nodes (count (g/nodes g))
     n-adopters (int (* proportion n-nodes))
     adopters (repeatedly n-adopters #(rand-nth (range n-nodes)))]
    (-> g
        (a/add-attr-to-all :green? 0)
        (a/add-attr-to-all :time-without 0)
        (a/add-attr-to-nodes :green? 1 adopters))))


(defn count-attr-for-neighbours
  "Sum up values for attribute attr for all neighbours of node in g."
  [g node attr]
  (reduce #(+ %1 (a/attr g %2 attr)) 0 (g/successors g node)))


(defn collect-attr-in-graph
  [g attr]
  (reduce #(conj %1 (a/attr g %2 attr)) [] (g/nodes g)))


(defn count-attr-in-graph
  [g attr]
  (reduce #(+ %1 (a/attr g %2 attr)) 0 (g/nodes g)))


(defn count-green-neighbours
  [g node]
  (count-attr-for-neighbours g node :green?))


(defdist decision-dist
  [g node]
  [n-neighbours (count (g/successors g node))
   n-green (count-green-neighbours g node)
   ratio (/ n-green n-neighbours)]
  (sample* [this]
    (if (zero? ratio)
      (let [param (sample* (uniform-continuous 0.01 0.1))]
        (sample* (bernoulli param)))
      (sample* (bernoulli (* 0.1 ratio))))))


(with-primitive-procedures [decision-dist]
  (defquery decision-dist-query [max-neighbours]
    (let [ns (range (inc max-neighbours))
          *sample (fn [green-neighbours]
                    (let [ratio (/ green-neighbours max-neighbours)]
                      (if (zero? ratio)
                        (let [param (sample (uniform-continuous 0.01 0.1))]
                          (sample* (bernoulli param)))
                        (sample (bernoulli (* 0.1 ratio))))))]
      {:decisions (reduce (fn [coll val] (conj coll (*sample val))) [] ns)})))


(with-primitive-procedures
  [count-green-neighbours g/successors a/attr a/add-attr decision-dist]
  (defm look-around
    "Decision function for the diffusion model. Every node in the graph looks
    at his neighbours and counts how many of them are already green. This number
    is used as a parameter for a probabilistic decision to buy a green car."
    [g node]
    (if (pos? (attr g node :green?))
      g
      (let
        [time (attr g node :time-without)
         ;n-neighbours (count (successors g node))
         ;green-neighbours (count-green-neighbours g node)
         ;;; new param to make decision harder
         ;param (* 0.001 (/ green-neighbours n-neighbours))

         ;; use custom dist
         green? (sample (decision-dist g node))
         ]

        (if (pos? green?)
          (add-attr g node :green? 1)
          (add-attr g node :time-without (inc time)))))))


(with-primitive-procedures
  [g/nodes]
  (defm diffusion-step
    [g]
    (reduce #(look-around %1 %2) g (nodes g))))


(with-primitive-procedures
  [count-attr-in-graph]
  (defm run-sim
    [t-max g]
    (loop [t 0
           graph g
           n-green []]
      (if (= t t-max)
        {:n-green n-green :graph graph}

        (recur (inc t)
               (diffusion-step graph)
               (conj n-green (count-attr-in-graph graph :green?)))))))


(with-primitive-procedures
  [count-green-neighbours count-attr-for-neighbours count-attr-in-graph collect-attr-in-graph]
  (defquery
    diffusion-query [graph]
    (let
      [t 50
       history (run-sim t graph)
       ]
      {:history history})))


(defn list-degrees [g]
  "Returns a vector where each element corresponds to the degree of a node in g."
  (reduce #(conj %1 (count (g/successors g %2))) [] (g/nodes g)))