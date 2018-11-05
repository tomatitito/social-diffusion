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

(def cli-opts
  [["-g" "--graph-type graph-type" "Type of graph used in simulation"
    :default "newman-watts"]
   ["-d" "--degree degree" "(Out-)Degree for nodes in graph"
    :default 5]
   ["-h" "--phi phi" "Parameter for adding edges to Newman-Watts graph"
    :parse-fn #(Float/parseFloat %)
    :default 0.3
    :validate [#(<= 0.0 % 1.0) "Parameter for adding edges to Newman-Watts graph has to be between 0 and 1"]]
   ["-n" "--n-samples n-samples" "Number of simulations run"
    :default 100
    :parse-fn #(Integer/parseInt %)]
   ["-N" "--n-nodes n-nodes" "Number of nodes in graph"
    :default 50
    :parse-fn #(Integer/parseInt %)]
   ["-a" "--algorithm algorithm" "Algortithm used for sampling from model"
    :default :smc
    :parse-fn #(keyword %)]
   ["-o" "--outfile"
    :required "Path to write results to"
    :id :outfile]
   ["-v" "--dotfile-dir"
    :required "Directory to write graphviz data to"
    :default "."
    :id :dotfile-dir]
   ])


(defn -main
  "Runs model and writes results according to arguments passed via command line."
  [& args]
  (let [parsed-args (parse-opts args cli-opts)
        graph-type (get-in parsed-args [:options :graph-type])
        n-nodes (get-in parsed-args [:options :n-nodes])
        degree (get-in parsed-args [:options :degree])

        ;; construct graph
        in-graph (-> (if (= graph-type "newman-watts")
                       (gen-newman-watts (g/graph) n-nodes degree (get-in parsed-args [:options :phi]))
                       (gen-barabasi-albert (g/graph) n-nodes degree))
                     (initialize-graph 0.1))

        ;; run model
        n-samples (get-in parsed-args [:options :n-samples])
        samples (doall (take n-samples (doquery :smc diffusion-query [in-graph] :number-of-particles 100)))
        ]
    ;; write results
    (if-let [dot-dir (get-in parsed-args [:options :dotfile-dir])]
      (do
        (io/dot in-graph (str dot-dir "/ch07-" graph-type "-before.dot"))
        (io/dot (m/from-result (first samples) [:history :graph]) (str dot-dir "/ch07-" graph-type "-after.dot"))))

    (if-let [outfile (get-in parsed-args [:options :outfile])]
      (dio/write-seasons! samples #(m/from-result % [:history :n-green]) outfile))))