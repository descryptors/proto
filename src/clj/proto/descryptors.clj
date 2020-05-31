(ns proto.descryptors
  (:require [proto.descryptors.defaults :as defaults]
            [proto.charts.util :refer [export-viz]]))



(def clipboard identity)


(defn descryptor-chart
  "General chart component."
  [data {:as opts :keys [view spec]}]
  
  (let [view (or view defaults/chart-period)]
    [:div.chart-wrapper
     (if-let [svg-data (get-in data [:svg view :data])]
       [:div.chart
        {:class (str (:class opts))}
        svg-data]
      
       [:div.chart {:class "chart--placeholder"}
        ;; empty space
        (export-viz {:size (:size spec)})])]))



(defn price-chart [price-data & [opts]]
  (descryptor-chart price-data (assoc opts :class "chart--price")))



(defn github-chart [github-data & [opts]]
  (descryptor-chart github-data (assoc opts :class "chart--github")))



(defn search-box [{:keys [search-icon]}]
  [:div.descryptor.search-card
   search-icon])
