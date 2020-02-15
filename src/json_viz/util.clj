(ns json-viz.util
  (:require
   [clojure.string        :as str]
   [contract-checker.core :as cc])
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
  [errors]
  (let [m  (group-by #(:path (val %)) (zipmap (range 1 1000) errors))
        m' (into {} (map (fn [[k v]] [k (into {} v)]) m))]
    m'))


(defn circled-number
  "Returns html code for the number n circled."
  ([n] (circled-number n "#FFF"))
  ([n color]
   (let [base 9311]
     (str "<FONT POINT-SIZE=\"12\" COLOR=\"" color  "\"><B>"
          "&#" (+ base n) ";</B></FONT>"))))


(defn nums->circled
  ([nums] (nums->circled nums 0))
  ([nums whitespace]
   (let [maj-col "#FF0000"
         min-col "#FF7200"
         m (reduce
            (fn [acc [k v]]
              (conj acc [k (if (= "Major" (:severity v)) maj-col min-col)]))
            []
            nums)]
     (str "<B>" (apply str (repeat whitespace "&nbsp;")) "</B>")
     (apply str
            (interpose "&nbsp;"
                       (map
                        #(circled-number (first %) (second %))
                        m))))))


(defn primitive?
  "Is the item a primitive type, i.e. not map, sequence or map-entry."
  [item]
  (not (or (map-entry? item) (sequential? item) (map? item))))


(defn primitives?
  [coll]
  (every? primitive? coll))


;; formatting for mult-line strings
(def linebreak "<BR />")


(defn bold [t] (str "<B>" t "</B>"))


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
    (apply str (interpose linebreak splits))))


(defn- has-words?
  [s]
  (if (string? s)
    (if (> (count (str/split s #" ")) 1)
      true
      false)
    false))


(def max-string-length 10)


(defn map->string
  "Creates a formatted string representation of the map."
  [m]
  (if (string? m) m ;; guard in case map not passed.
      (reduce
       (fn [acc [k v]]
         (if (has-words? v)
           (str acc  (bold k)  linebreak (split-string v max-string-length) linebreak)
           (str acc (bold k) "&nbsp;&nbsp;&nbsp;" v linebreak)))
       ""
       m)))


(defn seq->string
  "Creates a formatted string representation of the seq"
  [s]
  (reduce
   (fn [acc cur]
     (bold (str acc cur)))
   ""
   s))


(defn gv-wrap [html] (str "<" html ">"))
