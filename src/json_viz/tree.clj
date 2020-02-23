(ns json-viz.tree
  (:require [clojure.data.json     :as json]
            [contract-checker.core :as cc]
            [json-viz.rhizome      :as rhi]
            [clojure.string        :as str]
            [json-viz.util         :as util]
            [hiccup.core           :as hiccup])
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


;; html attrs
(def table-attr {"BORDER" 0 "ALIGN" "LEFT"})
(def td-attr {"ALIGN" "LEFT"})


(defn label [lbl-txt path highlights]
  (let [nums (get highlights path)
        circles (when nums (util/nums->circled nums))]
    (util/gv-wrap
     (hiccup/html
      [:TABLE table-attr
       [:TR
        [:TD td-attr lbl-txt]
        [:TD td-attr (if nums circles "&nbsp;")]]]))))


(defn js->dot
  "Returns dot representation of the json."
  [js options]
  (let [opts           (merge {:node1-options {:fillcolor "#dbdad6"}
                               :node2-options {:fillcolor "#c6e1f3"}
                               :highlight-nodes []
                               :highlight-options {:fillcolor "#e9c9c9"}
                               :style "tree"}
                              options)
        highlights     (util/highlight-map (:highlight-paths opts))
        js1            (util/with-path js [])]

    
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
                  {:label (label (util/seq->string (keys (dissoc n :path)))
                                 (:path n)
                                 highlights)}
                  (:node1-options opts)
                  (when (get highlights (:path n)) (:highlight-options opts)))
           ;; type 'node2'
           (merge graphviz-node-options
                  {:label (label (util/map->string (cc/node n))
                                 (:path n)
                                 highlights)}
                  (:node2-options opts)
                  (when (get highlights (:path n)) (:highlight-options opts)))))
       
       :options {:dpi 72}))))
