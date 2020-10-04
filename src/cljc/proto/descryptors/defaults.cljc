(ns proto.descryptors.defaults
  (:require [proto.util :refer [preload-resource]]
            [proto.charts.defaults :as pcd]))


(def coins-on-page 23)
(def visible-charts #{:github-chart :price-chart})


(def chart-period      :6m)
(def chart-size        [600 200])
(def price-chart-size  [600 200])
(def github-chart-size [26 7])

(def svg-path          [:svg :6m])

(def price-period-single :1m)
(def git-period-single   :1y)

(def github-precision
  (->> chart-period
       (get pcd/xgrid-spec)
       :precision))


(def mobile-breakpoint 750)


;; responsive dynamic chart sizes
;;
(def dynamic-chart-sizes
  {:price {:desktop [600 120]
           :mobile  [600 220]}
                  
   :git   {:desktop [60 6]
           :mobile [24 7]}})



(def static-price-charts
  [{:view chart-period
    :size price-chart-size
    :line-width "2px"}

   {:view price-period-single
    :line-width "1px"
    :xticks? true :yticks? true
    :grid-width "1px"
    :size (:desktop (:price dynamic-chart-sizes))}])



(def static-git-charts
  [{:view :6m
    :size github-chart-size}
   {:view :1y
    :size (:desktop (:git dynamic-chart-sizes))}])



(def themes #{:night-theme :day-theme})

(def theme-classes
  {:night-theme "night-mode-on"})
