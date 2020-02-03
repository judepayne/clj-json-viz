(ns json-viz.aws
  (:require [clojure.data.json  :as json]
            [clojure.string     :as str]
            [clojure.java.shell :as sh]
            [clojure.java.io    :as io]
            [json-viz.core      :as viz]))


(defn- format-error [s err]
  (apply str
    err "\n"
    (interleave
      (map
        (fn [idx s]
          (format "%3d: %s" idx s))
        (range)
        (str/split-lines s))
      (repeat "\n"))))


(def path-to-dot "/opt/bin/dot_static")
;(def path-to-dot "/usr/local/bin/dot")


(defn dot->svg
  "Takes a string containing a GraphViz dot file, and returns a string containing SVG."
  [s & {:keys [path] :or {path path-to-dot}}]
  (let [s' (str/replace s "\\\\n" "\n")     ;; for multi-line labels
        {:keys [out err]} (sh/sh path "-Tsvg" :in s')]
    (or
      out
      (throw (IllegalArgumentException. ^String (str "Graphviz!: "(format-error s' err)))))))


(defn xml->clean-svg
  "Cleans up svg emitted by Graphviz."
  [xml]
  (let [svg-start (str/index-of xml "<svg")
        svg (subs xml svg-start)]
    (-> svg
        (str/replace "\n" "")
        ;(str/replace "\"" "'")
        (str/replace "Monospace" "sans-serif")
        str/split-lines
        first)))


(defn js->dot-wrapper
  [js]
  (if (and (:json js) (:options js))
    ;; We have json & options. rearrange to call into js->dot
    (viz/js->dot (:json js) (:options js))

    ;; We just have json. pass in directly
    (viz/js->dot js {})))


(defn js->svg
  [js & {:keys [path] :or {path path-to-dot}}]
  (try
    (let [in (json/read-str js :key-fn keyword)
          dot (js->dot-wrapper in)
          svg (xml->clean-svg (dot->svg dot))]
      (json/write-str {:body svg}
                      :escape-slash false))
    (catch Exception e
      (println (str "Error!: " (.getMessage e)))
      (json/write-str {:error (str "Lambda function error: " (.getMessage e))}))))


;; test function
(comment
(spit "output.json" (:body (json/read-str (js->svg (slurp "example.json")) :key-fn keyword)))
)


;; aws lambda wrapper

(import com.amazonaws.services.lambda.runtime.Context)

(gen-class
 :name json_viz.Handler
 :prefix pch-
 :implements [com.amazonaws.services.lambda.runtime.RequestStreamHandler])


(defn pch-handleRequest [this input output ctx]
  (let [out (-> (slurp input)
                 js->svg)]
    (with-open [o (io/writer output)]
      (.write o out))))
