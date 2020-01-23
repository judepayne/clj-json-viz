(ns json-viz.rhizome
  (:require [rhizome.dot   :as rhi]))


(defn tree->dot
  "Like tree-seq, but returns a string containing a GraphViz dot file.  Additional options
   mimic those in graph->dot. Taken from Zac Tellman's wonderful rhizome library."
  [branch? children root
   & {:keys [vertical?
             node->descriptor
             edge->descriptor
             cluster->parent
             node->cluster
             cluster->descriptor
             options]
      :or {vertical? true
           node->descriptor (constantly {:label ""})
           edge->descriptor (constantly nil)
           node->cluster (constantly nil)
           cluster->parent (constantly nil)
           cluster->descriptor (constantly nil)}}]
  (let [node->children (atom {})
        nodes (tree-seq
                (comp branch? second)
                (fn [x]
                  (swap! node->children assoc x
                    (map vector
                      (repeatedly #(Object.))
                      (children (second x))))
                  (@node->children x))
                [(Object.) root])]
    (rhi/graph->dot nodes #(@node->children %)
      :directed? true
      :vertical? vertical?
      :options options
      :node->descriptor (comp node->descriptor second)
      :edge->descriptor (fn [a b] (edge->descriptor (second a) (second b)))
      :node->cluster (comp node->cluster second)
      :cluster->parent cluster->parent
      :cluster->descriptor cluster->descriptor)))
