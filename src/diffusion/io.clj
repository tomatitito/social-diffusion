(ns diffusion.io
  (:require [ministrants.core :as m]
            [clojure.java.io :as jio]
            [clojure.data.csv :as csv]))

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

(defn list-degrees [g]
  "Returns a vector where each element corresponds to the degree of a node in g."
  (reduce #(conj %1 (count (successors g %2))) [] (nodes g)))

(defn write-degrees-csv!
  [datavec outfile]
  (let [out (map vector datavec)]
    (with-open [writer (clojure.java.io/writer outfile)]
      (csv/write-csv writer out))))
;(m/from-result (first samples) [:history :n-green])
;(data-for-single-season (first samples) #(m/from-result % [:history :n-green] ) 0)
;(def seasons (m/from-results samples [:history :n-green]))
;(def time-without
;  (-> samples
;      (m/from-results [:history :graph])
;      (times-in-gs)
;      (flatten)
;      ))

