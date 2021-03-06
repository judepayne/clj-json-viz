(ns json-viz.uml
  (:require [clojure.data.json    :as json]
            [json-viz.rhizome     :as rhi]
            [clojure.string       :as str]
            [json-viz.util        :as util]
            [hiccup.core          :as hiccup]
            [json-viz.table       :as tbl]))

;; -------------- Navigation --------------
;; A. NODES

(defn one?
  "Is there one item inside the coll?"
  [coll]
  (= 1 (count coll)))


(def not-children [:type :enum :path])


(defn has-children?
  "Is this map-entry of type child (i.e. has further children)?"
  [item]
  (cond
    (map? item)            true
    (map-entry? item)      (let [[k v] item]
                             (if (util/contains? not-children k)
                               false
                               (if (or (map? v) (sequential? v))
                                 true
                                 false)))))


(defn entity-title
  "Returns the uml title of the node."
  [node]
  (cond
    (util/primitive? node)  node
    (map? node)        (:title node)
    (map-entry? node)  (name (key node))))


(defn- uml-type
  [item]
  (cond
    (map? item)          (get item :type "undefined")
    (sequential? item)   "array"
    (integer? item)      "integer"
    (number? item)       "number"
    (string? item)       "string"
    :else                "undefined"))


(defn- same?
  [items]
  (let [s (into #{} items)]
    (if (one? s) true false)))


(defn- entity-type-impl
  ([node] (entity-type-impl node []))
  ([node acc]
   (let [map-type (fn [m] (if-let [t (:type m)] t "object"))]
     (cond
       (map? node)        (let [t (map-type node)]
                            (if (= t "array")
                              (entity-type-impl (:items node) (conj acc "array"))
                              (conj acc (map-type node))))
       (map-entry? node)  (let [[k v] node]
                            (cond
                              (util/contains?
                               not-children k)      (conj acc (str v))
                              (string? v)           (conj acc v)
                              (number? v)           (conj acc (str v))
                              (map? v)              (entity-type-impl v acc)
                              (sequential? v)       (entity-type-impl v (conj acc "array"))))
       (sequential? node)  (if (same? (map uml-type node))
                             (conj acc (uml-type (first node)))
                             (conj acc "undefined"))))))


(defn entity-type
  "Return the uml type of the node."
  [node]
  (if (util/primitive? node) (uml-type node)
      (let [t (entity-type-impl node)]
        (case t
          nil      "-"
          (str
           (reduce (fn [acc cur] (str acc "[" cur "]")) t))))))



(defn entries
  "Returns the entries in a node"
  [node]
  (cond
    (map-entry? node)     (if (sequential? (val node))
                            (val node)
                            (into [] (val node)))
    (sequential? node)    node
    (map? node)           (into [] node)))


(defn- children
  "Returns the entries of the node which have further children."
  [node]
  (into [] (filter has-children? (entries node))))


(defn- branch?
  "True if the node is a branch?"
  [node]
  (not (empty? (children node))))


;; B. EDGES

(defn nav
  "Returns the node in the tree that the pointer points to.
   Note: As present can only handle ptr's beginning with root."
  [node ptr]
  (let [f (apply comp (reverse (mapv keyword (rest (str/split ptr #"/")))))]
    (f node)))


;; Brief documentation of the different ways Json-schema defines edges
;; i. Entity 1 (an array) is a parent of 2 & 1 has minItems & maxItems keys
;;    --> The multiplicty can be decoarated as a head label on parent e.g. 0..*
;;    --> There is no arrow on this relationship
;; ii. Entity 1 (object) is a parent of 2. This is a 'directed association' and
;;    can be represented by an edge with arrow 2 -> 1
;;   --> nothing to do here
;; iii. JSON schema pointers - the $ref key.
;; see  https://json-schema.org/understanding-json-schema/structuring.html

;; So iii.
;; alter the children function to detect :$ref, deref that reference to a node
;; and add that node to the set of children. This relationship 



;; -------------- Use Rhizome to produce dot --------------

(def ^:private graphviz-node-options
  {:fontsize 11
   :shape "plaintext"})


(defn- highlight-node?
  [path highlight-paths]
  (util/contains? highlight-paths path))


(defn- path
  "Returns the path"
  [nd]
  (cond
    (map-entry? nd)    (:path (val nd))
    :else (:path nd)))


(defn- filter-entries
  [filter-keys entries]
  (cond
    (map-entry? entries) (when (util/contains? filter-keys (key entries)) [entries])
    (util/primitives? entries) entries
    :else (filter (fn [[k v]] (not (util/contains? filter-keys k))) entries)))


(def ^:private dont-display [:path :type :title])


;; html attrs
(def table-attr {"ALIGN" "RIGHT" "BORDER" 1 "CELLBORDER" 0 "CELLSPACING" 0
                 "CELLPADDING" 7 "BORDER-COLOR" "#00FF00" "VALIGN" "TOP"})
(def hdr-attr {"ALIGN" "LEFT" "BORDER" 1 "SIDES" "B"})
(def td-attr {"ALIGN" "LEFT" "VALIGN" "TOP"})
(def hl-attr {"BGCOLOR" "#F5E2B8"})


(defn js->dot
  "Returns dot representation of the json, given a map of graphviz options."
  [js options]
  (let [opts           (merge {:node1-options {:fillcolor "#dbdad6"}
                               :node2-options {:fillcolor "#c6e1f3"}
                               :highlight-paths []
                               :highlight-options {:fillcolor "#f5e2b8"}}
                              options)
        highlights     (util/highlight-map (:highlight-paths opts))
        js1            (util/with-path js [])]

    (rhi/tree->dot
     branch?
     children
     js1

     :node->descriptor
     (fn [n]
       (let [hdr [(util/bold (entity-title n)) (util/bold (entity-type n))]
             hdr (util/decorate-item hdr hdr-attr hl-attr dont-display (path n) highlights)
             ps (mapv
                 (fn [item]
                   (util/decorate-item
                    [(entity-title item) (entity-type item)]
                    td-attr hl-attr
                    dont-display
                    (conj (path n) (entity-title item))
                    highlights))
                 (filter-entries dont-display (entries n)))]

         (merge graphviz-node-options
                {:label (tbl/table [hdr ps] 
                                   :table-attr (merge
                                                table-attr
                                                {"BGCOLOR" (-> opts :node2-options :fillcolor)}))})))

     :edge->descriptor (fn [_ _] {:dir "back"})

     :options {:dpi 72})))
