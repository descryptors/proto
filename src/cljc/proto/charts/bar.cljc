(ns proto.charts.bar
  (:require [thi.ng.geom.viz.core :as viz]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.vector :as v]
            [thi.ng.color.core :as col]
            [thi.ng.math.core :as m :refer [PI TWO_PI]]
            [proto.charts.util :refer [export-viz]]))



(defn bar-spec
  [data num width]
  (fn [idx col]
    {:values     data
     :attribs    {:stroke       col
                  ;; try percent instead
                  :stroke-width (str width "px")}
     :layout     viz/svg-bar-plot}))



(defn viz-spec
  [{{:as spec
     :keys [xdomain ydomain
            xsize   ysize
            xminor  yminor
            size]
     [width height] :size} :spec
    data :data}]
  
  (let [ ;;formatter (f/formatters :year-month-day)
        bar-width (float (/ width (count data)))]

    (->>
     {:data [((bar-spec data 0 bar-width) 0 "steelblue")]
      :x-axis (viz/linear-axis
               (cond-> {:domain xdomain
                        :range  [0 width]
                        :pos    height
                        ;;:label-style {:fill "darkgray"}
                        ;;:label  (viz/default-svg-label #(->> % c/from-long (f/unparse formatter)))
                        :attribs {:display :none}}
                 xminor (assoc :minor (/ xsize xminor))))
   
      :y-axis (viz/linear-axis
               (cond-> {:domain   ydomain
                        :range    [(* height 0.87) (* height 0.0625)]
                        :pos 0
                        :label    (viz/default-svg-label int)
                        :attribs  {:stroke "lightgray"
                                   :stroke-dasharray "1,1"
                                   :display :none}}
                 yminor (assoc :minor (/ ysize yminor))))
      :grid {:minor-y true
             :minor-x true}}

     (viz/svg-plot2d-cartesian)
     (assoc {:size size} :viz))))



(defn bar-chart
  [bar-data]
  (let [{:keys [size] :as viz} (viz-spec bar-data)]
    {:data (export-viz viz)
     :spec {:size size}}))


