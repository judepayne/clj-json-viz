(ns json-viz.util
  (:require [contract-checker.core :as cc])
  (:refer-clojure :exclude [contains?]))


(defn contains?
  [coll item]
 (cond
   (map? coll)  (clojure.core/contains? coll item)
   (sequential? coll) (some? (some #{item} coll))
   :default      false))


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

    (sequential? item)  (mapv #(with-path % path) item)

    :else               item))


(defn seq->str
  [s]
  (apply str (interpose ", " s)))


(defn make-highlight-map
  [paths f]
  (let [m (group-by val (zipmap (range 1 1000) paths))]
    (reduce (fn [acc [k v]]
              (assoc acc k (f (map first v))))
            {}
            m)))
