(ns proto.charts.line
  (:require [taoensso.timbre :refer [info]]
            [thi.ng.geom.viz.core :as viz :refer [axis-common* linear-scale select-ticks]]
            [thi.ng.geom.svg.core :as svg]
            [thi.ng.geom.vector :as v :refer [vec2]]
            [thi.ng.color.core :as col]
            [thi.ng.geom.utils :as gu]
            [thi.ng.geom.core :as g]
            [thi.ng.math.core :as m]
            [proto.charts.util :as pcu]
            [proto.charts.defaults :as pcd :refer [xgrid-spec ygrid-spec]]
            #?@(:clj [[clj-time.coerce :as ctc]
                      [clj-time.core :as ct]
                      [clj-time.format :as ctf]]
                :cljs [[cljs-time.coerce :as ctc]
                       [cljs-time.core :as ct]
                       [cljs-time.format :as ctf]
                       [goog.string :as gstring :refer [format]]
                       [goog.string.format]])))




(defn lin-tick-marks
  [[d1 d2] delta]
  (if (m/delta= delta 0.0 m/*eps*)
    '()
    (let [d2' (+ d2 m/*eps*)]
      (filter #(m/in-range? d1 d2' %)
              (range d1 (+ d2 delta) delta)))

    ;; original version
    #_(let [d2' (m/roundto d2 m/*eps*)]
        (filter #(m/in-range? d1 d2 (m/roundto % m/*eps*))
                (range d1 (+ d2 delta) delta)))))



(defn linear-axis
  [{:keys [domain range major minor] :as spec}]
  (let [major' (if major (lin-tick-marks domain major))
        minor' (if minor (lin-tick-marks domain minor))
        minor' (if (and major' minor')
                 (filter (complement (set major')) minor')
                 minor')]
    (-> spec
        (assoc
         :scale (linear-scale domain range)
         :major major'
         :minor minor')
        (axis-common*))))



(defn svg-axis-grid2d-cartesian
  [x-axis y-axis {:keys [attribs minor-x minor-y]}]
  (let [[x1 x2] (:range x-axis)
        [y1 y2] (:range y-axis)
        scale-x (:scale x-axis)
        scale-y (:scale y-axis)]
    (svg/group
     (merge {:stroke "#ccc" :stroke-dasharray "1 1"} attribs)
     (if (or (:major x-axis) (:minor x-axis))
       (map #(let [x (scale-x %)] (svg/line (vec2 x y1) (vec2 x y2))) (select-ticks x-axis minor-x)))
     (if (or (:major y-axis) (:minor y-axis))
       (map #(let [y (scale-y %)] (svg/line (vec2 x1 y) (vec2 x2 y))) (select-ticks y-axis minor-y))))))



;; don't display axis line
(defn svg-x-axis-cartesian
  [{:keys [scale major-size minor-size label-dist pos label] [r1 r2] :range
    :as spec}]
  (let [y-major (+ pos major-size)
        y-minor (+ pos minor-size)
        y-label (+ pos label-dist)]
    (viz/svg-axis*
     spec nil ;;(svg/line (vec2 r1 pos) (vec2 r2 pos))
     #(let [x (scale %)] (svg/line (vec2 x pos) (vec2 x y-major)))
     #(let [x (scale %)] (svg/line (vec2 x pos) (vec2 x y-minor)))
     #(label (vec2 (scale %) y-label) %))))



;; don't display axis line
(defn svg-y-axis-cartesian
  [{:keys [scale major-size minor-size label-dist label-y pos label] [r1 r2] :range
    :or {label-y 0}
    :as spec}]
  (let [x-major (- pos major-size)
        x-minor (- pos minor-size)
        x-label (- pos label-dist)]
    (viz/svg-axis*
     spec nil ;;(svg/line (vec2 pos r1) (vec2 pos r2))
     #(let [y (scale %)] (svg/line (vec2 pos y) (vec2 x-major y)))
     #(let [y (scale %)] (svg/line (vec2 pos y) (vec2 x-minor y)))
     #(label (vec2 x-label (+ (scale %) label-y)) %))))




(defn svg-plot2d-cartesian
  [{:as opts :keys [x-axis y-axis grid data attribs]
    :or {attribs {}}}]
  (let [opts (assoc opts :project v/vec2)]
    (svg/group
     attribs
     (if grid (svg-axis-grid2d-cartesian x-axis y-axis grid))
     (map (fn [spec] ((:layout spec) opts spec)) data)
     (if (:visible x-axis) (svg-x-axis-cartesian x-axis))
     (if (:visible y-axis) (svg-y-axis-cartesian y-axis)))))




(defn splodge-svg-label
  [f] (fn [p x]
        (svg/group
         {:opacity 0.8}
         (svg/defs
           [:filter {:x 0 :y 0 :width 1 :height 1 :id "solid"}
            [:feFlood {:flood-color "white"}]
            [:feComposite {:in "SourceGraphic"}]])
         
         ;;(svg/text p (f x))
         (svg/text p (f x) {:filter "url(#solid)"}))))




(defn add-ticks
  ([chart-data] (add-ticks {} chart-data))
  ([{:as opts :keys [screen xticks? yticks?]
     :or {screen :desktop}}
    {:as chart-data :keys [data spec]}]
  
   (let [{:keys [xdomain ydomain ysize]} spec
         [xstart xend] xdomain
         [ystart yend] ydomain

         view (:view spec)
        
         xmajor (when xticks?
                  (some-> (get xgrid-spec view)
                          (get-in [:major screen])))
        
         ymajor (when yticks?
                  (some->> (get-in ygrid-spec [:rows screen])
                           (/ ysize)))]
    
     (->>
      (cond-> {}
        xmajor
        (merge {:xmajor xmajor
                :xformatter (pcu/date-formatter (get-in xgrid-spec [view :formatter]))})
       
        ymajor
        (merge {:ymajor (/ (- yend ystart) (get-in ygrid-spec [:rows screen]))
                :yformatter pcu/compactnum}))
     
      (update chart-data :spec merge)))))




(defn viz-spec
  [{data :data
    {:as spec
     :keys [xdomain ydomain
            xsize   ysize attribs margin
            xminor  xmajor yminor ymajor
            ymargin ydist xmargin xdist
            size font-size xformatter yformatter
            formatter line-width font-size grid-width]
     [width height] :size
     [ystart yend] :ydomain
     :or {line-width "1.3px" grid-width "1px" attribs {}
          ymargin 55 ydist 25 xmargin 40 xdist 35
          font-size 8 xformatter identity
          yformatter (viz/value-formatter 2)}} :spec}]

  (let [line-width (if (number? line-width) (str line-width "px") line-width)
        ymargin (if ymajor ymargin 1)
        xmargin (if xmajor xmargin (if ymajor (/ font-size 2) 1))]
    (->>
     {:attribs attribs
      :x-axis (linear-axis ;;use our custom fn without rounding
               (cond-> {:domain xdomain
                        :range [ymargin (dec width)]
                        :pos (- height xmargin)
                        :label-dist xdist
                        :label (viz/default-svg-label xformatter)
                      
                        :label-style {:font-family "Pixelmix"
                                      :font-size font-size
                                      :fill "var(--gray-dark)"
                                      :text-rendering "geometricPrecision"}

                        :attribs {:stroke-dasharray 0
                                  :stroke-width grid-width
                                  :stroke "var(--gray-dark)"
                                  :shape-rendering "crispEdges"
                                  :class "ticks--x"}
                        :visible true}
              
                 xminor (assoc :minor xminor)
                 xmajor (assoc :major xmajor)))

   
      :y-axis (linear-axis
               (cond-> {:domain ydomain
                        :range [(- height xmargin) (/ font-size 2)]
                        :pos ymargin
                        :label-dist ydist
                        :label (viz/default-svg-label yformatter)
                      
                        :label-style {:text-anchor "end"
                                      :font-family "Pixelmix"
                                      :font-size font-size
                                      :style (str "transform:translateY("
                                                  (dec (int (/ font-size 2))) "px)")
                                      :fill "var(--gray-dark)"
                                      :text-rendering "geometricPrecision"}
                     
                        :attribs {:stroke-dasharray 0
                                  :stroke-width grid-width
                                  :stroke "var(--gray-dark)"
                                  :class "ticks--y"
                                  :shape-rendering "crispEdges"}
                        :visible true}
              
                 yminor (assoc :minor yminor)
                 ymajor (assoc :major ymajor)))
   
   
      :grid {:attribs {:stroke-dasharray "1 1"
                       :stroke-width grid-width
                       :stroke "var(--gray-dark)"
                       :shape-rendering "crispEdges"}}
   
   
      :data [{:values  data
              :attribs {:fill "#4682b4"
                        :stroke "#4682b4"
                        :stroke-width line-width
                        :stroke-linejoin :round
                        :stroke-linecap :round
                        :shape-rendering "geometricPrecision"}
              :layout  viz/svg-line-plot}]}

     svg-plot2d-cartesian
     (assoc {:size size} :viz))))



(defn line-chart
  [line-data]
  (let [{:as spec :keys [viz size]} (viz-spec line-data)]
    {:data (pcu/export-viz spec)
     :spec {:size size}}))





(defn price-charts
  [price-data chart-specs]
  (reduce
   (fn [data {:as opts :keys [view xticks? yticks?]}]
     (update-in
      data [:svg view]
      (fn [_]
        (when (pcu/enough-data? view data)
          (some-> (get data (get-in pcd/xgrid-spec [view :precision]))
                  (cond->>
                      :default (pcu/add-spec opts)
                      ;; add grid lines
                      (or xticks? yticks?)
                      (add-ticks {:xticks? xticks? :yticks? yticks?}))
                  line-chart)))))
   price-data
   chart-specs))
