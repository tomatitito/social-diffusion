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


;(oz/v! (histo-spec time-without))
;(oz/v! (season-plot-spec samples))
;
;(format-decisions-vega decisions)
;
;(defn format-decisions-csv
;  [decisions]
;  (as-> decisions v
;        (reduce (fn [one two] (map + one two)) (first v) (rest v))
;        (map #(/ % 1000.0) v)                                ;to get probabilities
;        (zipmap (range (count v)) v)
;        (partition 2 v)
;        (flatten v)
;        (partition 2 v)))
;
;(format-decisions-csv decisions)
;
;(oz/v! (histo-spec
;         (reduce (fn [one two] (map + one two)) (first decisions) (rest decisions))
;         ))
;
;(defn decision-spec
;  [decisions]
;  {:data     {:values (format-decisions-vega decisions)}
;   :mark     "bar"
;   :encoding {
;              :x {:field :n :type "ordinal"}
;              :y {:field :p :type "quantitative"}}})
;
;
;(oz/v! (decision-spec decisions) )
