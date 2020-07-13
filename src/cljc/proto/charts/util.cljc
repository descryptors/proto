(ns proto.charts.util
  (:require [thi.ng.geom.svg.core :as tsvg :refer [svg-attribs]]
            [thi.ng.color.core :as col]
            [thi.ng.math.core :as m]
            [taoensso.timbre :refer [info]]
            [clojure.pprint :refer [cl-format]]
            [proto.descryptors.defaults :as defaults]
            [proto.charts.defaults :as pcd :refer [xgrid-spec ygrid-spec]]
            #?@(:clj [[clj-time.coerce :as ctc]
                      [clj-time.format :as ctf]]
                :cljs [[cljs-time.coerce :as ctc]
                       [cljs-time.format :as ctf]
                       [goog.string :as gstring :refer [format]]])))



(defn svg
  [attribs & body]
  [:svg
   (svg-attribs
    attribs
    {"xmlns" "http://www.w3.org/2000/svg"
     "version" "1.1"})
   body])



#?(:cljs
   (defn export-viz [{:keys [viz size]}]
     (let [[w h] (or size defaults/chart-size)]
       (->> viz
            (svg {:width "100%"
                  :viewBox (str "0 0 " w " " h)})
            
            #_(svg (cond-> {:width (if (string? w) w (str w "px"))}
                     h (assoc :height (if (string? h) h (str h "px")))))))))


#?(:clj
   (defn export-viz [{:keys [viz size]}]
     (let [[w h] (or size defaults/chart-size)]
       (->> viz
            (svg {:width "100%"
                  :viewBox (str "0 0 " w " " h)})
            
            #_(svg (cond-> {:width (if (string? w) w (str w "px"))}
                     h (assoc :height (if (string? h) h (str h "px")))))
            
            (tsvg/serialize)))))




(defn chart-placeholder
  "Display a placeholder svg."
  [& [opts]]
  [:div.chart-wrapper
   [:div.chart.chart--placeholder
    (export-viz opts)]])




(defn ts-mean [ts]
  (->> ts
       ((juxt (comp (partial reduce +)
                    (partial map second))
              count))
       (apply /)))



(defn ts-variance [ts]
  (let [mean (ts-mean ts)
        amount' (dec (count ts))
        xs (map second ts)]
    (->> xs
         (map #(-> (- % mean)
                   (Math/pow 2)
                   (/ amount')))
         (reduce +)
         Math/sqrt)))






(def min-max  (juxt (partial reduce min) (partial reduce max)))
(def get-size (comp (partial apply -) reverse))
(def remove-nils (filter (partial not-any? nil?)))



(defn filter-period [[start end] data]
  (filter
   (fn [[timestamp _]]
     (m/in-range? start end timestamp))
   data))



(defn filter-period-xf [[start end]]
  (filter
   (fn [[timestamp _]]
     (m/in-range? start end timestamp))))



(defn add-spec
  "Calculate common values and add it to :spec. Enforce period
  with :view. Use `data-fn` to extract a coll of [x y] values."
  [{:keys [data spec] :as chart-data}
   & [{:as opts :keys [data-xf view size]
       :or {data-xf (map (juxt first second))}}]]

  ;; move cleanup to higher fn?
  (if-let [data (->> data
                     (sequence (comp data-xf remove-nils))
                     not-empty)]
    (let [[xstart xend]  (->> data (map first) min-max)
          xperiod  (some->> view (get xgrid-spec) :period)
          xdomain  (if xperiod
                     [(- xend xperiod) xend]
                     [xstart xend])

          data (if xperiod
                 (filter-period xdomain data)
                 data)

          [ystart yend] (->> data (map second) min-max)
           
          ydomain (if (= ystart yend)
                    [0 (inc yend)] ;;fixme: better way?

                    (if (< (ts-variance data) 0.005)
                      ;; low variance
                      (let [oney (* 2 (- yend ystart))]
                        [(max 0 (- ystart oney))  (+ yend oney)])

                      ;; normal
                      [ystart yend]))
           
          xsize (get-size xdomain)
          ysize (get-size ydomain)]

       
       
      (->> {:xdomain xdomain
            :ydomain ydomain
            :xsize xsize
            :ysize ysize}
            
           (merge (dissoc opts :data-fn))
     
           (update chart-data :spec merge)))
    
    chart-data))






(def suffixes ["" "K" "M" "B"])

(defn compactnum [num]
  (if (zero? num)
    num
    (let [[num suffix]
          (if (< num 50000)
            [num ""]
            (loop [num num
                   [suffix & rst] suffixes]
              (if (or (< num 1000)
                      (empty? rst))
                [(double num) suffix]
                (recur (/ num 1000) rst))))

          num (m/roundto num 0.001)
          
          [left right] ((juxt long #(- % (long %))) num)]
      
      (->
       (if (zero? left)
         (if (< right 1e-4)
           (cl-format nil "~,3,1,,'*E" right)
           (format "%.3f" right))
        
         (if (zero? right)
           num
           (cond
             (< left 10) (format "%.2f" num)
             ;;(< left 1000) (format "%.2f" num)
             :else (format "%.0f" num))))
       
       (str suffix)))))




#?(:clj
   (defn cmean [[^Double mean ^Long k] ^Double v]
     [(+ mean (/ (- v mean) (inc k)))
      (inc k)]))



#?(:clj
   (defn resample-period
     "From minute -> hour or minute -> day. Returns:
  [[(+ tstart prec), mean of semi-closed interval (tstart (+ tstart
  prec)]] [(+ tstart prec prec) ...] ...]
  `(resample-period 100 0 [[t v] ...])`"
     [prec tstart data]
     (loop [ts (sort-by first data)
            res []
            tspan nil
            tprev nil
            mean nil ;;[mean count]
            ]
    
       (if (not-empty ts)
         (let [[t v] (first ts)]
           (if (and tspan (<= t (+ tspan prec)))
             ;; keep meaning
             (recur (rest ts) res tspan t
                    (if mean
                      (cmean mean v)
                      [v 1.0]))
        
             (if tspan
               ;; finished current group
               (recur ts
                      (if mean
                        ;; got some data in this time span
                        (conj res [(+ tspan prec) (double (first mean))])
                        ;; no data for current span
                        res)
                      (+ tspan prec)
                      t
                      nil)

               ;; just starting
               (recur ts res tstart nil nil))))
      
         (when mean
           (if (== tprev (+ tspan prec))
             ;; last time span is complete
             (conj res [(+ tspan prec) (first mean)])
             ;; last span is not complete
             (not-empty res)))))))




#?(:clj
   (defn minute->precision
     "Returns new timeseries taken from :minute and adjusted to precision.
  `(minute->precision :price :hour coin)`"
     [data-key precision coin]
     (let [old-spec (:spec (add-spec (get-in coin [:data data-key precision])))
           new-spec (:spec (add-spec (get-in coin [:data data-key :minute])))
           
           [start filter-start]
           (if-let [old-start (second (:xdomain old-spec))]
             [old-start (inc old-start)]
             (repeat 2 (or (first (:xdomain new-spec)) 0)))
           
           end (or (second (:xdomain new-spec)) 0)]


       (when (< start end)
         (some->> (get-in coin [:data data-key :minute :data])
                  (filter-period [filter-start end])
                  distinct
                  (resample-period (get pcd/precisions precision) start)
                  (assoc-in {:slug (:slug coin)}
                            [:data data-key precision :data]))))))





#?(:clj
   (defn minute->precision2
     "Returns new timeseries taken from :minute and adjusted to precision.
  `(minute->precision2 (get-in coin [:data :price]) :hour)`"
     [precision data]
     (let [old-spec (min-max (keep first (get-in data [precision :data])))
           new-spec (min-max (keep first (get-in data [:minute :data])))
           
           [start filter-start]
           (if-let [old-start (second old-spec)]
             [old-start (inc old-start)]
             (repeat 2 (or (first new-spec) 0)))
           
           end (or (second new-spec) 0)]

       (cond-> data
         (< start end)
         (update-in [precision :data] (fnil into [])
                    (some->> (filter-period [filter-start end] (get-in data [:minute :data]))
                             distinct
                             (resample-period (get pcd/precisions precision) start)))))))






#?(:clj
   (defn resample-rf
     "Reducing fn for transducers, resampling data with step precision."
     [^Long step [^Long start ^Long end]]
     (fn
       ([] (transient {}))
    
       ([coll]
        (->> (persistent! coll)
             ;; [[t1 mean1] ...]
             (map (juxt first (comp first second)))
             ;; remove incomplete samples
             (filter #(<= (first %) end))))
    
       ([coll [^Long t ^Double v]]
        (let [ ;; t in semi-open interval (start (+ start step)] -> (+ start step)
              k (+ step (- (dec t) (mod (- (dec t) start) step)))
              mean (get coll k)]
          (assoc! coll k (if mean (cmean mean v) [v 1.0])))))))




#?(:clj
   (defn minute->precision3
     "Returns new timeseries taken from :minute and adjusted to precision.
  `(minute->precision3 :hour (get-in coin [:data :price]))`"
     [precision data]
     (let [old-spec (min-max (keep first (get-in data [precision :data])))
           new-spec (min-max (keep first (get-in data [:minute :data])))
           
           [start filter-start]
           (if-let [old-start (second old-spec)]
             [old-start (inc old-start)]
             (repeat 2 (or (first new-spec) 0)))
           
           end (or (second new-spec) 0)]

       ;; return only new data
       #_(when (< start end)
           (transduce
            (comp (filter-period-xf [filter-start end])
                  (distinct))
            (resample-rf (get pcd/precisions precision) [start end])
            (get-in data [:minute :data])))


       ;; merge new data into old data
       (cond-> data
         (< start end)
         (update-in [precision :data] (fnil into [])
                    (transduce
                     (comp (filter-period-xf [filter-start end])
                           (distinct))
                     (resample-rf (get pcd/precisions precision) [start end])
                     (get-in data [:minute :data])))))))







#?(:clj
   (defn trim-precision
     "Returns trimmed data from {:<data-key> {:<precision> {:data [...}}}
  `(trim-precision :price :hour :1m coin)`"
     [data-key precision view coin]
     (let [old-data (-> (get-in coin [:data data-key precision])
                        (add-spec {:view view}))]
       (when (not-empty (:spec old-data))
         (some->> (filter-period (:xdomain (:spec old-data))
                                 (:data old-data))
                  distinct
                  not-empty  vec
                  (assoc-in {:slug (:slug coin)}
                            [:data data-key precision :data]))))))




#?(:clj
   (defn trim-precision2
     "Returns trimmed data from {:<data-key> {:<precision> {:data [...}}}
  `(trim-precision :price :hour :1m coin)`"
     [data-key precision view coin]
     (let [old-data (-> (get-in coin [:data data-key precision])
                        (add-spec {:view view}))]
       (when (not-empty (:spec old-data))
         (some->> (:data old-data)
                  (transduce (comp (filter-period-xf (:xdomain (:spec old-data)))
                                   (distinct))
                             conj)
                  not-empty
                  (assoc-in {:slug (:slug coin)}
                            [:data data-key precision :data]))))))



#?(:clj
   (defn trim-precision3
     [period chart-data]
     (if-let [xdomain (->> (add-spec chart-data {:view period})
                           :spec :xdomain)]
       (update chart-data :data (partial filter-period xdomain))
       chart-data)))





(defn enough-data?
  "Check if there is enough data for at least half of view period."
  [data view]
  (let [{:keys [precision period]} (get pcd/xgrid-spec view)]
    (some-> (map first (get-in data [precision :data]))
            not-empty
            min-max
            get-size
            (/ period)
            (>= 0.5))))



(defn enough-data-spec?
  "Check if there is enough data (based on spec) for at least half of
  view period."
  [data view]
  (let [{:keys [precision period]} (get pcd/xgrid-spec view)]
    (some-> (get-in data [precision :spec :xsize])
            (/ period)
            (>= 0.5))))





(defn date-formatter [formatter]
  (fn [x]
    (->> (ctc/from-long x)
         (ctf/unparse #?(:cljs {:format-str formatter}
                         :clj (ctf/formatter formatter))))))
