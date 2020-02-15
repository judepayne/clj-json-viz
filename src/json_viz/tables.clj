(ns json-viz.tables
  "A namespaces for producing graphviz html-like tables, useful
  in graphviz for both node and edge labels."
  (:require [clojure.string       :as str]
            [json-viz.util        :as util]))


;; html-like labels for graphviz -----------------


(defn- attr
  [attrs]
  (when attrs
    (apply str
           (interleave (repeat " ")
                       (for [[k' v'] attrs]
                         (str k' "=\"" v' "\""))))))


(defn k->disp
  [k]
  (if (util/primitive? k)
    (str "  " k)
    (str "  " (name k))))


(defn- row
  [item td-attrs
   & {:keys [conv-key?] :or {conv-key? true}}]
  (let [{[k v] :item nums :highlights hl-row-attrs :hl-row-attrs} item
        at (attr (merge td-attrs hl-row-attrs))]
    (when (not (and (nil? k) (nil? v)))
      (str "<TR>"
           "<TD " at ">" (if conv-key? (k->disp k) k) ":  " v "</TD>"
           "<TD VALIGN=\"BOTTOM\"" at ">" (when nums (util/nums->circled nums)) "</TD>"
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
  (let [highlights (get highlight-items item-path)]
    (if highlights
      {:item item
       :highlights highlights
       :hl-row-attrs highlight-row-attrs}
      {:item item})))


(defn table
  "Produces a graphviz html-like node label, given:
  - the header-item, a 2-tuple of the key and value making up the header row.
  - row-items, a sequence of 2-tuples each of the key and value making up (each) row.
  - (optional) highlight-rows, a a map {item-to-highlight, (error) numbers}, highlighted with..
  - (optional) highlight-rows-bgcolor, a hex color value used as background color highlighting.
  - (optional) td-attrs & table-attrs, an map of (html/ graphviz) attributes and their
    values to be added into the table tag & TD tags of every one of the row-items."
  [header-item props objs
   & {:keys [path
             highlight-items
             highlight-items-attr
             td-attrs
             last-row-attrs
             last-section-attrs
             table-attrs]
      :or {path []
           highlight-items {}
           highlight-items-attr {"BGCOLOR" "#e0a19b"}
           td-attrs nil
           last-row-attrs nil
           last-section-attrs nil
           table-attrs nil}}]
  (println props)
  ;; We 'decorate' items with any meta data so they can pass through intermediate functions to 'row'
  (let [dec-fn (fn [item item-path]
                 (decorate-item item item-path highlight-items highlight-items-attr nil))
        hdr (dec-fn header-item path)
        props (map (fn [[k v]]
                     (dec-fn [k v] (conj path k)))
                   props)
        objs (map (fn [item] {:item item}) objs)]
    (println props)
    (str
     "<<TABLE " (attr table-attrs) ">"
     (header-row hdr)
     (sections [props objs]
               :td-attrs td-attrs
               :last-row-attrs last-row-attrs
               :last-section-attrs last-section-attrs)
     "</TABLE>>")))
