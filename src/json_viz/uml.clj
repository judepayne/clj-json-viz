(ns json-viz.uml
  (:require [clojure.data.json     :as json]
            [json-viz.rhizome      :as rhi]
            [clojure.string       :as str]
            [json-viz.util        :as util]))


(defn primitive?
  [item]
  (not (or (map-entry? item) (sequential? item) (map? item))))


(defn primitives?
  [coll]
  (every? primitive? coll))


;; -------------- UML class display --------------

;; html-like labels for graphviz -----------------

(defn circled-number
  "Returns html code for the number n circled."
  [n]
  (let [base 9311]
    (str "&nbsp;&#" (+ base n) ";")))


(defn nums->circled
  [nums]
  (apply str (map circled-number nums)))


(defn- attr
  [attrs]
  (when attrs
    (apply str
           (interleave (repeat " ")
                       (for [[k' v'] attrs]
                         (str k' "=\"" v' "\""))))))


(defn k->disp
  [k]
  (if (primitive? k)
    (str "  " k)
    (str "  " (name k))))


(defn- row
  [item td-attrs
   & {:keys [conv-key?] :or {conv-key? true}}]
  (let [{[k v] :item nums :nums hl-row-attrs :hl-row-attrs} item
        at (attr (merge td-attrs hl-row-attrs))]
    (when (not (and (nil? k) (nil? v)))
      (str "<TR>"
           "<TD " at ">" (if conv-key? (k->disp k) k) ":  " v "</TD>"
           "<TD " at ">" (when nums (nums->circled nums)) "</TD>"
           "</TR>"))))


(defn- empty-row
  [td-attrs]
  (str "<TR>"
       "<TD " (attr td-attrs) ">&nbsp;</TD>"
       "<TD " (attr td-attrs) "></TD>"
       "</TR>"))


(defn- rows
  "Produces a string for an html table rows from items. Each item should be
  a [k v] vector tuple.
  highlight-items should be in the form of a map {item-to-highlight [k v], (error) numbers}"
  [items
   & {:keys [td-attrs last-attrs conv-key?] :or {td-attrs nil last-attrs nil conv-key? true}}]
  (if (empty? items)
    (empty-row last-attrs)
    (let [first-items (butlast items)
          last-item   (last items)]
      (str
       (apply str
              (for [item first-items] (row item td-attrs :conv-key? conv-key?)))
       (row last-item (merge td-attrs last-attrs) :conv-key? conv-key?)))))


(defn sections
  "Multiple row sets, where sections is a collection of rows"
  [sections
   & {:keys [td-attrs last-row-attrs last-section-attrs]
      :or {td-attrs nil last-row-attrs nil last-section-attrs nil}}]
  (let [first-sections (butlast sections)
        last-section   (last sections)]
    (str     
     (apply str (map #(rows % :td-attrs td-attrs :last-attrs last-row-attrs) first-sections))
     (rows last-section :td-attrs td-attrs :last-attrs (merge last-row-attrs last-section-attrs)))))


(defn- header-row
  [item]
  (row item (merge {"ALIGN" "CENTER" "BORDER" "1" "SIDES" "B"})
       :conv-key? false))


(defn decorate-item
  [item item-path highlight-items highlight-row-attrs highlight-num-attrs]
  (let [nums (get highlight-items item-path)]
    (if nums
      {:item item
       :nums nums
       :hl-row-attrs highlight-row-attrs}
      {:item item})))


(defn uml-class
  "Produces a graphviz html-like node label, given:
  - the header-item, a 2-tuple of the key and value making up the header row.
  - row-items, a sequence of 2-tuples each of the key and value making up (each) row.
  - (optional) highlight-rows, a a map {item-to-highlight, (error) numbers}, highlighted with..
  - (optional) highlight-rows-bgcolor, a hex color value used as background color highlighting.
  - (optional) td-attrs & table-attrs, an map of (html/ graphviz) attributes and their
    values to be added into the table tag & TD tags of every one of the row-items."
  [header-item props objs path
   & {:keys [highlight-items
             highlight-items-attr
             td-attrs
             last-row-attrs
             last-section-attrs
             table-attrs]
      :or {highlight-items {}
           highlight-items-attr {"BGCOLOR" "#e0a19b"}
           td-attrs nil
           last-row-attrs nil
           last-section-attrs nil
           table-attrs nil}}]
  ;; We 'decorate' items with any meta data so they can pass through intermediate functions to 'row'
  (let [dec-fn (fn [item item-path]
                 (decorate-item item item-path highlight-items highlight-items-attr nil))
        hdr (dec-fn header-item path)
        props (map (fn [[k v]]
                     (dec-fn [k v] (conj path k)))
                   props)
        objs (map (fn [item] {:item item}) objs)]
    (str
     "<<TABLE " (attr table-attrs) ">"
     (header-row hdr)
     (sections [props objs]
               :td-attrs td-attrs
               :last-row-attrs last-row-attrs
               :last-section-attrs last-section-attrs)
     "</TABLE>>")))



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
    (map? node)        (:title node)
    (map-entry? node)  (name (key node))))


(defn uml-type
  [item]
  (cond
    (map? item)          (get item :type "undefined")
    (sequential? item)   "array"
    (integer? item)      "integer"
    (number? item)       "number"
    (string? item)       "string"
    :else                "undefined"))


(defn same?
  [items]
  (let [s (into #{} items)]
    (if (one? s) true false)))


(defn entity-type-impl
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
  (if (primitive? node) (uml-type node)
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


(defn entry-title
  "Returns the title of the child."
  [item]
  (cond
    (primitive? item)    item
    :else (name (key item))))


(defn children
  "Returns the entries of the node which have further children."
  [node]
  (into [] (filter has-children? (entries node))))


(defn branch?
  "True if the node is a branch?"
  [node]
  (not (empty? (children node))))

;; Need representation for oneOf, allOf and anyOf.
;; treat as arrays with no name


;; B. EDGES

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

(def graphviz-node-options
  {:fontsize 11
   :shape "plaintext"})


(defn highlight-node?
  [path highlight-paths]
  (util/contains? highlight-paths path))


(def table-attrs
  {"BORDER" 1
   "CELLBORDER" 0
   "CELLSPACING" 0
   "CELLPADDING" 4})


(defn path
  [nd]
  (cond
    (map-entry? nd)    (:path (val nd))
    :else (:path nd)))


;; BREAKS ON ENTRY_TITLE



(defn filter-entries
  [filter-keys entries]
  (cond
    (map-entry? entries) (when (util/contains? filter-keys (key entries)) [entries])
    (primitives? entries) entries
    :else (filter (fn [[k v]] (not (util/contains? filter-keys k))) entries)))


(def dont-display [:path :type :title])


(defn js->dot
  "Returns dot representation of the json."
  [js options]
  (let [opts           (merge {:node1-options {:fillcolor "#dbdad6"}
                               :node2-options {:fillcolor "#c6e1f3"}
                               :highlight-paths []
                               :highlight-options {:fillcolor "#f5e2b8"}}
                              options)
        highlight-map  (util/make-highlight-map (:highlight-paths opts) identity)
        js1            (util/with-path js [])
        highlight-attr (fn [path paths]
                         (when (highlight-node? path paths)
                           (merge (:highlight-options opts)
                                  {:xlabel (get highlight-map path)})))]

    (rhi/tree->dot
     branch?
     children
     js1
     :node->descriptor
     (fn [n]
       (let [hdr [(entity-type n) (entity-title n)]
             ps (mapv
                 (fn [item] [(entry-title item) (entity-type item)])
                 (filter-entries dont-display (entries n)))
             cn []]
         (merge graphviz-node-options
                {:label
                 (uml-class hdr ps cn (path n)
                            :td-attrs {"ALIGN" "LEFT"}
                            :last-row-attrs {"ALIGN" "LEFT" "BORDER" "1" "SIDES" "B"}
                            :last-section-attrs {"BORDER" "0"}
                            :table-attrs (merge table-attrs
                                                {"BGCOLOR" (-> opts :node2-options :fillcolor)})
                            :highlight-items highlight-map
                            :highlight-items-attr {"BGCOLOR"
                                                      (-> opts :highlight-options :fillcolor)})})))

     :edge->descriptor (fn [_ _] {:dir "back"})

     :options {:dpi 72})))
