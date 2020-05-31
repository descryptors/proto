(ns proto.descryptors.defaults
  (:require [proto.util :refer [preload-resource]]
            [proto.charts.defaults :as pcd]))


(def coins-on-page 23)
(def visible-charts #{:github-chart :price-chart})


(def chart-period      :6m)
(def chart-size        [600 200])
(def price-chart-size  [600 200])
(def svg-path          [:svg :6m])
(def github-chart-size [26 7])

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


(def static-charts
  [{:view chart-period
    :size price-chart-size
    :line-width "2px"}

   {:view :1y
    :line-width "1px"
    :xticks? true :yticks? true
    :grid-width "1px"
    :size (get-in dynamic-chart-sizes [:price :desktop])}])


(def themes #{:night-theme :day-theme})

(def theme-classes
  {:night-theme "night-mode-on"})

