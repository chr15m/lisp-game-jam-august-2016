(ns oglo.util
  (:refer-clojure :exclude [slurp])
  (:use markdown.core))

(defmacro pre-markdown [file]
  (md-to-html-string (clojure.core/slurp file)))
