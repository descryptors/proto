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

(def price-period-single :1d)
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
    :size (get-in dynamic-chart-sizes [:price :desktop])}])


(def themes #{:night-theme :day-theme})

(def theme-classes
  {:night-theme "night-mode-on"})



(def src-logo     "/img/logo.svg")
(def src-homepage "/img/homepage.svg")

(def svg-logo          (preload-resource "public/img/logo.svg"))
(def svg-homepage      (preload-resource "public/img/homepage.svg"))
(def svg-intersection  (preload-resource "public/img/search-intersection.svg"))
(def svg-union         (preload-resource "public/img/search-union.svg"))
(def svg-info          (preload-resource "public/img/info.svg"))
(def svg-theme         (preload-resource "public/img/theme.svg"))
(def svg-edit          (preload-resource "public/img/edit.svg"))
(def svg-arrow-up      (preload-resource "public/img/arrow-up.svg"))
(def svg-loading       (preload-resource "public/img/loading.svg"))
(def svg-search        (preload-resource "public/img/search.svg"))
(def svg-founder       (preload-resource "public/img/founder.svg"))
(def svg-prev          (preload-resource "public/img/prev.svg"))
(def svg-next          (preload-resource "public/img/next.svg"))
(def svg-mobile-search (preload-resource "public/img/mobile-search.svg"))
(def svg-mobile-info   (preload-resource "public/img/mobile-info.svg"))
(def svg-mobile-charts (preload-resource "public/img/mobile-charts.svg"))
(def svg-mobile-sort   (preload-resource "public/img/mobile-sort.svg"))
(def svg-mobile-theme  (preload-resource "public/img/mobile-theme.svg"))
(def svg-close         (preload-resource "public/img/close.svg"))
(def svg-close-thin    (preload-resource "public/img/close-thin.svg"))
(def svg-comment       (preload-resource "public/img/comment.svg"))
