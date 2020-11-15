(ns challenge.core
  (:require [challenge.vm :refer [run]])
  (:gen-class))

(defn -main
  [& args]
  (run (first args)))
