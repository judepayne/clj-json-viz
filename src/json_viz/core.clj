(ns json-viz.core
  (:require [clojure.data.json     :as json]
            [rhizome.dot           :as rhi]
            [contract-checker.core :as cc]
            [clojure.string        :as str]))


;; For visualization of a json-schema

(defn- split-map
  "Splits a map of n entries into a sequence of n 1-entry maps."
  [m]
  (reduce
   (fn [acc [k v]]
     (conj acc {k v}))
   []
   m))


(defn- children
  "Returns the children of the node."
  [n]
  (cond
    (sequential? n) n

    (map? n) (if (empty? (cc/node n))
               ;; i.e. structural entries only
               (let [f (val (first n))]    ;; - TODO change to handle n > 1
                 (if (sequential? f) f
                     (split-map f)))
               ;; split out the structural elements
               (split-map (cc/structural n)))))


(defn- split-string
  "Splits a string with a newline every n charcters. Only splits on space."
  [s n]
  (let [words (str/split s #" ")
        stringize (fn [parts] (apply str (interpose " " parts)))
        splits (loop [prev-splits []
                      cur-split []
                      cur-count 0
                      word (first words)
                      rest-words (rest words)]
                 (if (empty? rest-words)
                   (conj prev-splits (stringize (conj cur-split word)))
                   (if (> (+ cur-count (count word)) n)
                     (recur
                      (conj prev-splits (stringize (conj cur-split word)))
                      []
                      0
                      (first rest-words)
                      (rest rest-words))
                     (recur
                      prev-splits
                      (conj cur-split word)
                      (+ cur-count (count word))
                      (first rest-words)
                      (rest rest-words)))))]
    (apply str (interpose "\n" splits))))


(defn has-words?
  [s]
  (if (string? s)
    (if (> (count (str/split s #" ")) 1)
      true
      false)
    false))


(def max-string-length 10)


(defn- map->string
  "Creates a formatted string representation of the map."
  [m]
  (if (string? m) m ;; guard in case map not passed.
      (reduce
       (fn [acc [k v]]
         (if (has-words? v)
           (str acc k "\n" (split-string v max-string-length) "\n")
           (str acc k " " v "\n")))
       ""
       m)))


(defn- seq->string
  "Creates a formatted string representation of the seq"
  [s]
  (reduce
   (fn [acc cur]
     (str acc cur))
   ""
   s))


(def graphviz-node-options
  {:style "filled, rounded"
   :fontsize 10
   :shape "rect"})


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



(defn js->dot
  "Returns dot representation of the json."
  [js
   & {:keys [color1
             color2]
      :or {color1 "snow"
           color2 "lightsteelblue1"}}]
  (if (and (empty? (cc/node js)) (> (count js) 1))
    (js->dot {"{ }" js}) ;; catch when first node only has structural elements.
    (tree->dot
     (fn [n] (not (empty? (cc/structural n))))
     children
     js
     :node->descriptor (fn [n] (if (empty? (cc/node n))
                                 (merge graphviz-node-options {:label (seq->string (keys n))
                                                               :fillcolor "#dbdad6"})
                                 (merge graphviz-node-options {:label (map->string (cc/node n))
                                                               :fillcolor "#c6e1f3"})))
     :options {:dpi 72})))
