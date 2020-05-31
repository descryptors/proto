(ns proto.charts.matrix
  (:require [thi.ng.geom.viz.core :as viz]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.core   :as g]
            [thi.ng.geom.utils  :as gu]
            [thi.ng.color.core  :as col]
            [thi.ng.color.gradients :as grad]
            [thi.ng.math.core :as m :refer [PI TWO_PI]]
            [proto.charts.util :refer [export-viz]]
            [proto.charts.defaults :refer [day week periods]]))



(def fill-size 0.75)


(def start-color (col/hex->int "#eaf0f2"))
(def end-color   (col/hex->int "#4682b4"))

(def coefficients (grad/cosine-coefficients start-color end-color))



(defn commits-per-day
  "[epoch commit] -> [day commit],
  day starting from 0."
  [t0 commits]
  (for [[t1 n] commits
        :let [t (- t1 t0)
              d (int (/ t day))]]
    [d n]))

;;get size and average data



(defn commits-day->matrix
  [commits & [rows]]
  (let [cols (-> (last commits) first (/ rows) Math/ceil)
        mat (->> (repeat (* rows cols) 0)
                 (into [])
                 (transient))]

    (doseq [[d n] commits]
      (assoc! mat d n))
    
    {:rows rows
     :cols cols
     :data (persistent! mat)}))



(defn circle-cell
  [a b c d col]
  (svg/circle
   (gu/centroid [a b c d])
   (* 0.5 fill-size (g/dist a b)) {:fill col}))


(defn filter-period [xs t]
  (let [end (first (last xs))]
    (filter #(< (- end (first %)) t) xs)))


(def views {:6m {:data-fn #(filter-period % (* (:6m periods) day))
                 :view-fn commits-per-day
                 :mat-fn  commits-day->matrix
                 :rows 7}

            :1y {:data-fn #(filter-period % (* (:1y periods) day))
                 :view-fn commits-per-day
                 :mat-fn  commits-day->matrix
                 :rows 11}
            
            :all {:data-fn identity
                  :view-fn commits-per-day
                  :mat-fn  commits-day->matrix
                  :rows 11}

            ;; as example
            :1w {:data-fn #(-> (take-last 7 %)
                               (filter-period week))
                 :view-fn commits-per-day
                 :mat-fn  commits-day->matrix
                 :rows 1}})




(defn svg-heatmap
  "Custom viz/svg-heatmap, without ndarray."
  [{:keys [x-axis y-axis project]}
   {:keys [matrix value-domain clamp palette palette-scale attribs shape]
    :or {value-domain  [0.0 1.0]
         palette-scale viz/linear-scale
         shape         #(svg/polygon [%1 %2 %3 %4] {:fill %5})}
    :as d-spec}]
  (let [scale-x (:scale x-axis)
        scale-y (:scale y-axis)
        pmax    (dec (count palette))
        scale-v (palette-scale value-domain [0 pmax])
        {:keys [rows cols data]} matrix]
    (apply svg/group
           attribs
           (for [x (range cols)
                 y (range rows)
                 :let [v (nth data (+ y (* x rows)))]]
             (shape
              (project [(scale-x x) (scale-y y)])
              (project [(scale-x (inc x)) (scale-y y)])
              (project [(scale-x (inc x)) (scale-y (inc y))])
              (project [(scale-x x) (scale-y (inc y))])
              (palette (m/clamp (int (scale-v v)) 0 pmax)))))))



(defn viz-spec
  [{data :data {:as spec :keys [size view]} :spec}]
  (let [{:keys [data-fn view-fn mat-fn rows]} (get views view)
        data      (data-fn data)
        [from to] (viz/value-domain-bounds (map first data))
        view-data (view-fn from data)
        rows      (or (when-let [[w h] size]
                        (some-> (or (get periods view)
                                    (count view-data))
                                (* h) (/ w)
                                Math/sqrt
                                (m/roundto 1)
                                ;; Math/ceil
                                ))
                      (:rows spec) rows)
        mat       (mat-fn view-data rows)
        cols      (:cols mat)
        size      [cols rows]]

    (->>
     {:x-axis (viz/linear-axis
               {:domain [0 cols]
                :range  [0 cols]
                :visible false
                ;;:attribs {:display :none}
                })
      
      :y-axis (viz/linear-axis
               {:domain  [0 rows]
                :range   [0 rows]
                :visible false
                ;; :attribs {:display :none}
                })
      
      :data   [{:matrix        mat
                :value-domain  [0 (reduce max (:data mat))]
                :palette       (grad/cosine-gradient 100 coefficients)
                
                :palette-scale viz/linear-scale ;;(partial viz/log-scale 3)
                :layout        svg-heatmap
                :shape         circle-cell}]}
     
     (viz/svg-plot2d-cartesian)
     (assoc {:size size} :viz))))



(defn matrix-chart
  [github-data]
  (let [{:as spec :keys [size viz]} (viz-spec github-data)]
    {:data (export-viz spec)
     :spec {:size size}}))



;; todo: fit data independent of number of days by averaging.

