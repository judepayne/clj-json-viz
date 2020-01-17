#!/bin/bash

lein clean

lein uberjar

aws lambda update-function-code \
--function-name  VisualizeJson \
--zip-file fileb://target/clj-json-viz-0.1.0-standalone.jar
