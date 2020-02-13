(ns json-viz.tree
  (:require [clojure.data.json     :as json]
            [contract-checker.core :as cc]
            [json-viz.formatting   :as fmt]
            [json-viz.rhizome      :as rhi]
            [clojure.string        :as str]
            [json-viz.util         :as util])
  (:refer-clojure :exclude [contains?]))


;; visualisation of json-schema

(defn- split-map
  "Splits a map of n entries into a sequence of n 1-entry maps."
  [m]
  (let [path (:path m)]
    (reduce
     (fn [acc [k v]]
       (conj acc {k v :path (conj path (name k))}))
     []
     (dissoc m :path))))


(defn- children
  "Returns the children of the node."
  [n]
  (cond
    (sequential? n) n

    (map? n) (if (empty? (cc/node n))
               ;; i.e. structural entries only
               (let [f (val (first n))]    ;; - TODO change to handle n > 1
                 (if (sequential? f)
                   f ;; these are where path is trapped
                   (split-map f)))
               ;; split out the structural elements
               (split-map (cc/structural n)))))


(def graphviz-node-options
  {:style "filled, rounded"
   :fontsize 10
   :shape "rect"})


(defn highlight-node?
  [path highlight-paths]
  (util/contains? highlight-paths path))


(defn js->dot
  "Returns dot representation of the json."
  [js options]
  (let [opts           (merge {:node1-options {:fillcolor "#dbdad6"}
                               :node2-options {:fillcolor "#c6e1f3"}
                               :highlight-nodes []
                               :highlight-options {:fillcolor "#f2c9c9"}
                               :style "tree"}
                              options)
        highlight-map  (util/make-highlight-map (:highlight-paths opts) util/seq->str)
        js1            (util/with-path js [])
        highlight-attr (fn [path paths]
                         (when (highlight-node? path paths)
                           (merge (:highlight-options opts)
                                  {:xlabel (get highlight-map path)})))]
    
    (if (and (empty? (cc/node js)) (> (count js1) 1))

      (js->dot {"{ }" js1} opts) ;; catch when first node only has structural elements.

      (rhi/tree->dot
       (fn [n] (not (empty? (cc/structural n))))
       children
       js1
       :node->descriptor
       (fn [n]
         ;; tree style diagram
         (if (empty? (cc/node n))
           ;; This is a structural node - type 'node1'
           (merge graphviz-node-options
                  {:label (fmt/seq->string (keys (dissoc n :path)))}
                  (:node1-options opts)
                  (highlight-attr (:path n) (:highlight-paths opts)))
           ;; type 'node2'
           (merge graphviz-node-options
                  {:label (fmt/map->string (cc/node n))}
                  (:node2-options opts)
                  (highlight-attr (:path n) (:highlight-paths opts)))))

       :options {:dpi 72}))))
