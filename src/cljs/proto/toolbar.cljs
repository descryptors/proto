(ns proto.toolbar
  (:require [reagent.core :as r]
            [proto.util :as u :refer [inline-html limiter]]
            [proto.descryptors.defaults :as defaults]))



(defn toggler
  [& [{:keys [on? ratelimit] :or {ratelimit 200}}]]
  
  (let [state (r/atom on?)
        click-on-update? (atom false)
        limiter (u/limiter ratelimit)]
    
    (r/create-class
     {:display-name "proto.toolbar.toggler"
      :component-did-update
      (fn [this]
        (when @click-on-update?
          (when-let [post-click (-> (r/props this) (get :post-click))]
            (post-click @state))
          (reset! click-on-update? false)))

      
      :reagent-render
      (fn [{:as props :keys [on-click active-class]} child]
        [:div
         (-> props
             (dissoc :active-class :on? :on-click :post-click)
             (merge
              {:key child
               :class (str (:class props) " " (when @state active-class))
               :on-click #(limiter
                           (fn [e]
                             (cond-> (swap! state not)
                               on-click (on-click))
                             ;; make sure we re-render and then run
                             ;; post-click actions
                             (reset! click-on-update? true)))}))
         child])})))




(defn radio-button
  [{:as opts :keys [label on-click on?]}]

  [toggler {:active-class "active-color"
            :class (or (:class opts) "toolbar__filter")
            :post-click on-click
            :on? on?
            :key (str label "button")}
   label])



(defn button
  [{:as opts :keys [label on-click on?]}]

  [toggler {:active-class ""
            :class (str (:class opts))
            :post-click on-click
            :on? on?
            :key (str label "button")}
   label])



(defn label [label]
  [:span.toolbar__label label])


(defn info-icon [& [opts]]
  [toggler
   (->> opts
        (merge {:active-class "active-color-svg"
                :class "toolbar__item icon icon--info"
                :key "info-icon"}
               (inline-html defaults/svg-info)))])


(defn theme-icon [& [opts]]
  [toggler
   (->> opts
        (merge {:active-class "active-color-svg"
                :class "toolbar__item icon icon--theme"
                :key "theme-icon"}
               (inline-html defaults/svg-theme)))])



(defn vertical-break []
  [:div.toolbar__item.break.break--vertical])


(defn horizontal-break []
  [:div.toolbar__item.break.break--diagonal])


(defn diagonal-break []
  [:div.toolbar__item.break.break--diagonal])

