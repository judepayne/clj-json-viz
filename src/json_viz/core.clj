(ns json-viz.core
  (:require [clojure.data.json     :as json]
            [contract-checker.core :as cc]
            [json-viz.formatting   :as fmt]
            [json-viz.rhizome      :as rhi])
  (:refer-clojure :exclude [contains?]))



(defn contains?
  [coll item]
 (cond
   (map? coll)  (clojure.core/contains? coll item)
   (sequential? coll) (some? (some #{item} coll))
   :default      false))


;; html-like labels for graphviz -------------------------------------
;; We can produce uml class like diagrams using Graphviz' html like labels feature
;; https://www.graphviz.org/doc/info/shapes.html#html
(defn- td-attr
  [td-attrs]
  (when td-attrs
    (apply str
           (interleave (repeat " ")
                       (for [[k' v'] (partition 2 td-attrs)]
                         (str k' "=\"" v' "\""))))))


(defn- rows
  "Produces a string for an html table rows from items. Each item should be
  a [k v] vector tuple."
  [items highlight-items highlight-bgcolor
   & {:keys [td-attrs] :or {td-attrs nil}}]
  (apply str
         (for [[k v] items]
           (str
            (if (contains? highlight-items [k v])
              (str "<TR><TD BGCOLOR=\"" highlight-bgcolor "\"" (td-attr td-attrs))
              (str "<TR><TD" (td-attr td-attrs)))
            ">" k ": " v "</TD></TR>"))))


(defn- header-row
  [item]
  (rows [item] [] nil :td-attrs ["ALIGN" "CENTER"]))


(defn table
  "Produces a graphviz html-like node label, given:
  - the header-item, a 2-tuple of the key and value making up the header row.
  - row-items, a sequence of 2-tuples each of the key and value making up (each) row.
  - (optional) highlight-rows, a sequence of 2-tuples representing any rows to be highlighted by..
  - (optional) highlight-rows-bgcolor, a hex color value used as background color highlighting.
  - (optional) td-attrs, an even-numbered sequence of (html/ graphviz) attributes and their
    values to be added into the TD tag of every one of the row-items."
  [header-item row-items
   & {:keys [highlight-rows
             highlight-rows-bgcolor
             td-attrs]
      :or {highlight-rows []
           highlight-rows-bgcolor "#e0a19b"
           td-attrs nil}}]
  (str
   "<TABLE>"
   (header-row header-item)
   (rows row-items highlight-rows highlight-rows-bgcolor :td-attrs td-attrs)
   "</TABLE>"))


;; end html-like labels section --------------------------------------

(defn with-path
  "Decorates (in metadata) each level of a nested structure with :path."
  [item path]
  (cond
    (map? item)         (let [chdn (cc/structural item)
                              decorated (assoc item :path path)]
                          (if (empty? chdn)
                            decorated
                            (merge decorated
                                   (into {} (map #(with-path % path) chdn)))))
                        
    (map-entry? item)   [(key item)
                         (with-path (val item) (conj path (name (key item))))]

    (sequential? item)  (map #(with-path % path) item)

    :else               item))


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
  (contains? highlight-paths path))


(defn seq->str
  [s]
  (apply str (interpose ", " s)))


(defn make-highlight-map
  [paths]
  (let [m (group-by val (zipmap (range 1 1000) paths))]
    (reduce (fn [acc [k v]]
              (assoc acc k (seq->str (map first v))))
            {}
            m)))


(defn js->dot
  "Returns dot representation of the json."
  [js options]
  (let [opts (merge {:node1-options {:fillcolor "#dbdad6"}
                     :node2-options {:fillcolor "#c6e1f3"}
                     :highlight-nodes []
                     :highlight-options {:fillcolor "#f2c9c9"}}
                    options)
        highlight-map (make-highlight-map (:highlight-paths opts))
        js1 (with-path js [])
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
       :node->descriptor (fn [n] (if (empty? (cc/node n))
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
