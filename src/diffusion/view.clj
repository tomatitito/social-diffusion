(ns diffusion.view
  (:require [loom.attr :as a]
            [loom.graph :as g]
            [ministrants.core :as m]))


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


(defn season-plot-spec
  [samples]
  (let [seasons (m/from-results samples [:history :n-green])
        formatted (vec->vega-time-series seasons)]
    {:data     {:values formatted}
     :mark     "tick"
     :encoding {:x {:field :week :type "ordinal"}
                :y {:field :data :type "quantitative"}}
     }))

(defn colorize
  "Adds labels for coloring when plotting with graphviz."
  [g]
  (for [n (g/nodes g)]
    (if (zero? (a/attr g n :green?))
      (a/add-attr g n "color" "black")
      (a/add-attr g n "color" "yellow"))))
