(ns export
  "Writes Vega-Lite figure spec to notebooks/specs/<id>.vl.json with its data inlined.
   Run with: clojure -M:export"
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [specs]))

(def out-dir "notebooks/specs")

(defn- ->json-key
  "Vega-Lite wants the literal property '$schema'."
  [k]
  (let [s (name k)]
    (if (= s "schema") "$schema" s)))

(defn -main [& _]
  (let [data (-> "notebooks/data.edn" slurp edn/read-string)]
    (io/make-parents (io/file out-dir "x"))
    
    (doseq [[id {:keys [build]}] specs/all]
      (let [file (io/file out-dir (str id ".vl.json"))]
        (with-open [w (io/writer file)]
          (json/write (build data) w :key-fn ->json-key))
        (println "wrote" (str file))))
    
    (with-open [w (io/writer (io/file out-dir "manifest.json"))]
      (json/write (into {} (for [[id {:keys [render]}] specs/all]
                             [id (name render)]))
                  w))
    
    (println "wrote" (str (io/file out-dir "manifest.json")))))
