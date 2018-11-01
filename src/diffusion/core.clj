(ns diffusion.core
  (:require [loom.graph :as g]
            [loom.io :as io]
            [oz.core :as oz]
            [ministrants.core :as m]
            [anglican.core :refer [doquery]]
            [diffusion.model :refer [diffusion-query initialize-graph]]
            [diffusion.io :as dio]
            [diffusion.gen :refer :all]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))


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

(def cli-opts
  [["-g" "--graph-type graph-type" "Type of graph used in simulation"
    :default "newman-watts"]
   ["-d" "--degree degree" "(Out-)Degree for nodes in graph"]
   ["-h" "--phi phi" "Parameter for adding edges to Newman-Watts graph"
    :parse-fn #(Float/parseFloat %)
    :default 0.3
    :validate [#(<= 0.0 % 1.0) "Parameter for adding edges to Newman-Watts graph has to be between 0 and 1"]]
   ["-n" "--n-samples n-samples" "Number of simulations run"
    :default 100]
   ["-N" "--n-nodes n-nodes" "Number of nodes in graph"
    :default 50]
   ["-a" "--algorithm algorithm" "Algortithm used for sampling from model"
    :default :smc
    :parse-fn #(keyword %)]
   ["-o" "--outfile"
    :required "Path to write results to"
    :id :outfile]])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [parsed-args (parse-opts args cli-opts)
        graph-type (get-in parsed-args [:options :graph-type])
        n-nodes (get-in parsed-args [:options :n-nodes])
        degree (get-in parsed-args [:options :degree])
        gen-fn (symbol (str "gen-" graph-type))
        in-graph (if (= graph-type "newman-watts")
                   (gen-newman-watts (g/graph) n-nodes degree phi))
        n-samples (get-in parsed-args [:options :n-samples])
        samples (doall (take n-samples (doquery diffusion-query [in-graph] :number-of-particles 100)))]
    (println gen-fn)
    )
  )
