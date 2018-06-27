(ns diffusion.core
  (:require [loom.graph :as g]
            [loom.attr :as a]
            [loom.io :as io]
            [clojure.java.io :as jio]
            [clojure.data.csv :as csv]
            [loom.gen :as gen]
            [oz.core :as oz]
            [ministrants.core :as m])
  (:use [anglican [core :exclude [-main]] runtime emit])
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


(def adj-list {0 [1 4] 1 [0 2 3] 2 [1 3 4] 3 [1 2] 4 [0 2 5 6] 5 [4 6] 6 [4 5]})


(def g
  (-> path
      (create-graph-from-snap-edge-list )
      (initialize-graph 0.01)))

(def g2
  (->
    (gen-newman-watts (g/graph) 1000 10 0.3)
    (initialize-graph 0.1)))

(def g3
  (->
    (gen-newman-watts (g/graph) 10 2 0.1)
    (initialize-graph 0.1)))

(def samples (doall (take 1000 (doquery :smc diffusion-query [g2] :number-of-particles 1000))))
;(first samples)
;(count (g/nodes g))

(defn from-maps
  "Extracts all vals for key(s) ks from a coll of maps. ks must be given as
   a vector. Returns a vector."
  [coll ks]
  (reduce #(conj %1 (get-in %2 ks)) [] coll))


(defn extract-values
  [raw-results kw]
  (reduce #(conj %1 (get-in %2 [:result kw])) [] raw-results))


(defn times-in-g
  [g]
  (reduce #(conj %1 (a/attr g %2 :time-without)) [] (g/nodes g)))


(defn times-in-gs
  "Compute vega-lite-spec for histogram of time until green."
  [gs]
  (reduce #(conj %1 (times-in-g %2)) [] gs))

(defn histo-spec
  [data]
  {:data {:values data}
   :mark "bar"
   :encoding {:x {:field :data :type "quantitative" }
              :y {:aggregate "count" :type "quantitative"}}})


(defn vec->vega-time-series
  "Converts a vector of values. Returns a vector of maps, which have two key-value pairs,
  one for :week and one for :data (cases). This format can be supplied as a values vector
  for vega-lite."
  [vec]
  (letfn [(vec->series
            [v]
            (let [steps (range (count v))
                  steps-and-vals (zipmap steps v)]
              (for [[t v] steps-and-vals]
                {:week t :data v}))
            )
          ]
    (flatten
      (map
        #(vec->series %)
        vec))))


(defn filter-by-step
  "Takes anglican samples and returns only those for specified timestep."
  [samples step]
  (let
    [values-by-step (vec->vega-time-series (extract-values samples :history))
     week-only (filter #(= step (get % :week)) values-by-step)]
    week-only))


(defn week-histo-spec
  "Returns a spec for vega-lite to plot a histogram of new infections for a specified week."
  [samples week]
  {:data     {:values (from-maps (filter-by-step samples week) [:data])}
   :mark     "bar"
   :encoding {:x {:field "data" :type "quantitative"}
              :y {:aggregate "count" :type "quantitative"}}})


;(def history (vec->vega-time-series (extract-values samples :history)))
;
;(def history-plot
;  {:data {:values history}
;   :mark "tick"
;   :encoding {:x {:field :week :type "ordinal"}
;              :y {:field :data :type "quantitative"}}})

;(as-> samples v
;    (m/from-results v [:history :n-green])
;      (vec->vega-time-series v)
;    (take 2 v))

(defn season-plot-spec
  [samples]
  (let [seasons (m/from-results samples [:history :n-green])
        formatted (vec->vega-time-series seasons)]
    {:data     {:values formatted}
     :mark     "tick"
     :encoding {:x {:field :week :type "ordinal"}
                :y {:field :data :type "quantitative"}}
     }))

;;(oz/v! (week-histo-spec samples 1))

;
;(oz/v! (histo-spec (repeatedly 100 #(sample* (beta 0 3)))))
;
;(defn dashboard
;  [samples]
;  (let
;    [greens (histo-spec (extract-values samples :green))
;     times (->
;             (extract-values samples :times)
;             (flatten)
;             (histo-spec))
;     board {:hconcat [greens times]}
;     ]
;    (oz/v! board)))
;
;
;(dashboard samples)

(defmulti #^{:private true} data-for-single-season (fn [query-result f sim-id] (sequential? f)))

(defmethod data-for-single-season false [query-result f sim-id]
  (let
    [weeks (range (count (m/from-result query-result [:history :n-green])))
     cases (f query-result)
     sim-ids (repeat (count weeks) sim-id)]

    (partition 3
               (interleave weeks cases sim-ids))))


(defmethod data-for-single-season true [query-result f sim-id]
  (let
    [weeks (range (count (m/from-result query-result [:history :n-green])))
     sim-ids (repeat (count weeks) sim-id)
     cases (map #(%1 query-result) f)]

    (as-> cases v
          (apply interleave v)
          (partition (count cases) v)
          (interleave weeks v sim-ids)
          (flatten v)
          (partition (+ (count cases) 2) v))))

(defn write-seasons!
  "Takes output generated from anglican and writes data for seasons generated
  by getter-fn to outfile. An additional header can be specified. getter-fn can
  be a single function or a vector of functions, which can be used to simply
  collect data from the output or to compute values based on them. Note that
  these functions have to operate on a single sample, since they are called
  recursively inside a loop."
  [samples getter-fn outfile & header]
  (letfn
    [(csv-data [samples]
       (loop [coll []
              from-query samples
              n 0]

         (if (not (seq from-query))
           coll

           (let
             [single-sample (first from-query)
              csv-dat (data-for-single-season single-sample getter-fn n)]

             (recur (apply conj coll csv-dat)
                    (rest from-query)
                    (inc n))))))]

    (with-open [writer (jio/writer outfile)]
      (when header
        (csv/write-csv writer header))
      (csv/write-csv writer (csv-data samples)))))

(write-seasons! samples #(m/from-result % [:history :n-green]) "ngreen.csv")
;(m/from-result (first samples) [:history :n-green])
;(data-for-single-season (first samples) #(m/from-result % [:history :n-green] ) 0)
;(def seasons (m/from-results samples [:history :n-green]))
(def time-without
  (-> samples
      (m/from-results [:history :graph])
      (times-in-gs)
      (flatten)
      ))

(oz/v! (histo-spec time-without))
(oz/v! (season-plot-spec samples))

(defn write-decisions! [decs outfile]
  (with-open [writer (jio/writer outfile)]
    (csv/write-csv writer decs)))

(def decision-samples (doall (take 1000 (doquery :smc decision-dist-query [20] :number-of-particles 100))))
(def decisions (m/from-results decision-samples [:decisions]))
(take 4 decisions)
(map + (first decisions) (second decisions))
(def red (reduce (fn [one two] (map + one two)) (first decisions) (rest decisions)))

(defn format-decisions-vega
  [decisions]
  (as-> decisions v
        (reduce (fn [one two] (map + one two)) (first v) (rest v))
        (map (fn [key val] {:n key :p val}) (range (count v)) v)))

(format-decisions-vega decisions)

(defn format-decisions-csv
  [decisions]
  (as-> decisions v
        (reduce (fn [one two] (map + one two)) (first v) (rest v))
        (map #(/ % 1000.0) v)                                ;to get probabilities
        (zipmap (range (count v)) v)
        (partition 2 v)
        (flatten v)
        (partition 2 v)))

(format-decisions-csv decisions)

(oz/v! (histo-spec
         (reduce (fn [one two] (map + one two)) (first decisions) (rest decisions))
         ))

(defn decision-spec
  [decisions]
  {:data     {:values (format-decisions-vega decisions)}
   :mark     "bar"
   :encoding {
              :x {:field :n :type "ordinal"}
              :y {:field :p :type "quantitative"}}})


(oz/v! (decision-spec decisions) )
(write-decisions! (format-decisions-csv decisions) "decisions.csv")
