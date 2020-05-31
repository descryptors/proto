(ns proto.charts.defaults)


;; ms
(def minute (* 60 1000))
(def hour (* 60 minute))
(def day  (* 24 hour))
(def week (* 7 day))

;; how many days
(def periods
  {:1d 1 :1w 7 :1m 30 :6m 180 :1y 360})

(def precisions {:1d minute
                 :1w hour
                 :1m hour
                 :6m day
                 :1y day
                 :minute minute
                 :hour hour
                 :day day})



(def xgrid-spec
  (->> {:1d {:cols {:desktop 6
                    :sm 5
                    :xs 3}
             :formatter "hh:mma"
             :precision :minute
             :days 1}

        :1w {:cols {:desktop 7
                    :sm 5
                    :xs 3}
             :formatter "EEE"
             :precision :hour
             :days 7}

        :1m {:cols {:desktop 6
                    :sm 5
                    :xs 3}
             :formatter "D MMM"
             :precision :hour
             :days 30}

        :6m {:cols {:desktop 6
                    :sm 5
                    :xs 4}
             :formatter "D MMM"
             :precision :day
             :days 180}

        :1y {:cols {:desktop 6
                    :sm 5
                    :xs 4}
             :formatter "D MMM"
             :precision :day
             :days 360}}


       ;; add :period in ms
       ;; todo: rename to :period-ms
       (reduce-kv
        (fn [m k v]
          (->> (assoc v :period (* (get periods k) day))
               (assoc m k)))
        {})
       
       ;; add :major delta
       (reduce-kv
        (fn [m k {:as v :keys [period cols]}]
          (->> cols
               ((juxt :desktop :sm :xs))
               (map (partial / period))
               (zipmap [:desktop :sm :xs])
               (assoc v :major)
               (assoc m k)))
        {})))



(def ygrid-spec {:rows {:desktop 3
                        :sm 2
                        :xs 1}})
