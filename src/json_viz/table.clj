(ns json-viz.table
  (:require [json-viz.util        :as util]
            [hiccup.core          :as hiccup]))


(defn row [item]
  [:TR
   (for [x (:tds item)]
     [:TD (:attr item) x])])


(defn table [sections & {:keys [table-attr] :or {table-attr {}}}]
  (util/gv-wrap
   (hiccup/html
    [:TABLE table-attr
     (for [x sections]
       (cond
         (map? x)          (row x)
         (sequential? x)   (for [r x] (row r))))])))
