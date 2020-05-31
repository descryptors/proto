(require '[clojure.java.shell :as sh]
         '[figwheel.main.watching :as fww])

(println "watching Sass...")

(defn compile-sass [& args]
  (sh/sh "sass" "sass/main.sass:resources/public/css/main.css")
  (sh/sh "postcss" "resources/public/css/main.css" "-u" "autoprefixer"
         "-o" "resources/public/css/prefixed-main.css"))

(fww/add-watch!
 :proto-sass
 {:paths ["sass"]
  :filter (fww/suffix-filter #{"sass"})
  :handler (fww/throttle 50 compile-sass)})

(compile-sass)
