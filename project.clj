(defproject clj-json-viz "0.1.0"
  :description "A clojure library for visualizing json, with aws lambda wrapper"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/data.json "0.2.7"]
                 [com.amazonaws/aws-lambda-java-core "1.1.0"]
                 [rhizome-cljc "0.1.2"]
                 [contract-checker "0.1.0"]
                 [hiccup "1.0.5"]]
  :repl-options {:init-ns json-viz.core}
  :aot :all)
