(ns json-viz.formatting
  (:require  [clojure.string        :as str])
  (:refer-clojure :exclude [contains?]))


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
