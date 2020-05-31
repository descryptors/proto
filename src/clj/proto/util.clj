(ns proto.util
  (:require [clojure.java.io :as io]
            [linked.core :as linked])
  (:import [java.io PushbackReader]))



(defmacro preload-file [file]
  (slurp file))



(defn- read-one [r]
  (try
    (clojure.edn/read {:readers *data-readers*} r)
    (catch java.lang.RuntimeException e
      (if (= "EOF while reading" (.getMessage e))
        ::EOF
        (throw e)))))



(defn read-edn-seq [path & [f]]
  (with-open [r (PushbackReader. (io/reader (or (io/resource path) path)))]
    (let [coll (take-while #(not= ::EOF %) (repeatedly #(read-one r)))]
      (cond
        (fn? f) (doseq [item coll] (f item))
        (keyword? f) (doall (map f coll))
        :else (doall coll)))))




(defmacro preload-edn [file]
  (->> (read-edn-seq file)
       vec))



(defn load-resource [file]
  (slurp (or (io/resource file) file)))



(defmacro preload-resource [file]
  (let [data (load-resource file)]
    `~data))



(defmacro cc [args]
  `~args)



(defmacro inline-html [html & args]
  `{:dangerouslySetInnerHTML
    {:__html (str ~html ~@args)}})
