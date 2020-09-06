(ns proto.devcards
  (:require [devcards.core]
            [reagent.core :as r]
            [reagent.dom :as rd]
            [dommy.core :as d]
            [taoensso.timbre :refer [info]]
            [clojure.edn]
            [proto.mixins :refer [window-event-listener]]
            [thi.ng.math.core :as m]
            [proto.util :as u :refer [inline-html]]
            [proto.figwheel :refer [coins]]
            [proto.descryptors :as des :refer [github-chart price-chart
                                               search-box toggle-theme!
                                               editable gen-captcha]]
            [proto.dropdown :refer [dropdown]]
            [proto.toolbar :refer [button]]
            [proto.charts.util :as pcu :refer [chart-placeholder]]
            [proto.charts.line :as line]
            [proto.charts.defaults :as cd]
            [thi.ng.geom.svg.core :as tsvg]
            [thi.ng.geom.viz.core :as viz]
            [proto.charts.matrix :as matrix]
            [goog.string :refer [format]]
            [goog.string.format]
            [proto.descryptors.defaults :as defaults]
            [proto.descryptors.common :as common :refer [spacefy logo footer
                                                         expandable founder]]
            [proto.descryptors.samples :as samples]
            [bouncer.core :as b]
            [bouncer.validators :as bv]
            
            [cljs-time.coerce :as ctc]
            [cljs-time.core :as ct]
            [cljs-time.format :as cf]
            ;;[overlayscrollbars-react :refer [OverlayScrollbarsComponent]]
            )
  
  (:require-macros [devcards.core :as dc :refer [defcard-rg defcard]]))


(def editable-state (r/atom ""))

(def text "fdafdsfdsff\ndfgdgd\nfsdfdsf\nfsdfdsaf\nfsdfsdf")

#_(defn msgbox []
  [:> OverlayScrollbarsComponent
   {:options {:textarea {:dyn-height true}}}
   [:textarea {:style {:resize :none}
               :placeholder "fdafdsfdsff\ndfgdgd\nfsdfdsf\nfsdfdsaf\nfsdfsdf"}]
   
   #_[des/editable {:id "msg"
                    :tag :textarea
                    :class "form__textarea"
                    :style {:color :black }
                    :state editable-state
                    ;;:error (:message @errors)
                    ;;:on-focus
                    #_(fn [evt]
                        (on-focus :message evt)
                        (update-contact-message-error nil))
                    ;;:disabled @disabled?
                    :name "user_message"
                    :placeholder "Message"}]])


#_(defcard-rg message-box
    [msgbox])





#_(defcard-rg scrolls
  [:div {:style {:width 100 :height 100}}
   [:> OverlayScrollbarsComponent
    {:options {:scrollbars {:auto-hide :leave}}}
    [:div {:style {:width 60 :height 30}}
     [:p
      "Hello there Hello there Hello there Hello there Hello there Hello there Hello there Hello there Hello there Hello there Hello there Hello there Hello there Hello there Hello there Hello there Hello there Hello there Hello there Hello there Hello there Hello there Hello there Hello there"]]]])






(defcard-rg github
  [github-chart (-> coins (nth 4) :data :github)
   {:period-selectors? true
    :dynamic? true
    :view :1y
    :spec {:size (get-in defaults/dynamic-chart-sizes [:git :desktop])}}])







#_(defn clipboard []
  [:div
   [des/clipboard
    [:a.footer__support-tag
     {:data-clipboard-text "<<<< what link >>>>>"}
     "ETH"]]

   [des/clipboard
    [:a.footer__support-tag
     {:data-clipboard-text "<<<< another link >>>>>"}
     "BTC"]]])



#_(defcard-rg clipboard
  clipboard)





(defcard-rg notice
  [:div {:style {:background-color :blue}}
   [des/cookie-notice]])





(defcard-rg team
  [:div.single
   [common/team samples/team]])






(defcard-rg wallets
  [:div.single
   [common/wallets samples/wallets]])






(def exchanges-coin (r/atom (nth coins 0)))

(defcard-rg exchanges
  [:div.single
   [common/exchanges (:symbol @exchanges-coin)
    (get-in @exchanges-coin [:data :exchanges])
    #_samples/exchanges]

   #_[common/social-media "Reddit News"
      samples/reddit-news]

   #_[common/social-media "Latest Tweets"
      samples/tweets]])















(bv/defvalidator validate-captcha
  {:default-message-format "invalid captcha"}
  [captcha]
  (= (:answer des/*captcha*) (clojure.edn/read-string captcha)))




(def valid-contact
  [:email [[bv/required]
           [bv/email]]
   :message bv/required
   :captcha [[bv/required]
             [validate-captcha]]])


(binding [des/*captcha* {:answer 33}]
  (->> valid-contact
       (apply b/validate {:email "some.mail@gmail.com"
                          :message "hello"
                          :captcha "33"})))




(defn contact-form [{:as props :keys [on-close on-submit]}]
  (let [state (r/atom {:disabled? false
                       :step :send
                       :errors {}
                       :captcha (gen-captcha)})
        
        disabled? (r/cursor state [:disabled?])
        errors (r/cursor state [:errors])
        
        on-focus (fn [id evt] (swap! state update :errors dissoc id))]
    
    (fn []
      [:div {:style {:background-color :black}} ;;.form__wrapper
       ;; form input items
       (into
        [:form] ;;.form {:class (str "form--" (name @step))}
        
        (case (:step @state)
          :sent
          [[:div.sent]]
          
          [[:div.form__group.form__group--half
            [:div.form__first
             [editable {:type "text"
                        :tag :input
                        :class "form__input"
                        :state (r/cursor state [:values :first-name])
                        :disabled @disabled?
                        :id "name"
                        :name "user_name"
                        :placeholder "First Name"}]]
            [:div.form__last
             [editable {:type "text"
                        :tag :input
                        :class "form__input"
                        :state (r/cursor state [:values :last-name])
                        :disabled @disabled?
                        :id "name"
                        :name "user_name"
                        :placeholder "Last Name"}]]]

           [:div.form__item.form__email
            [editable {:type "email"
                       :tag :input
                       :class "form__input"
                       :state (r/cursor state [:values :email])
                       :error (:email @errors)
                       :on-focus (partial on-focus :email)
                       :disabled @disabled?
                       :id "mail"
                       :name "user_email"
                       :placeholder "Email"}]]

           [:div.form__item.form__company
            [editable {:type "text"
                       :tag :input
                       :class "form__input"
                       :state (r/cursor state [:values :company])
                       :disabled @disabled?
                       :id "company"
                       :name "user_company"
                       :placeholder "Company"}]]
           
           [:div.form__item.form__message
            [des/scrollbars
             [editable {:id "msg"
                        :tag :textarea
                        :class "form__textarea"
                        :state (r/cursor state [:values :message])
                        :error (:message @errors)
                        :on-focus (partial on-focus :message)
                        :disabled @disabled?
                        :name "user_message"
                        :placeholder "Message"}]]]

           [:div.form__group.form__group--last
            [:div.form__capcha-wrapper
             [:label.form__label.form__label--capcha {:for "capcha"} (:question (:captcha @state))]
             [editable {:id "capcha"
                        :class "form__input form__input--capcha"
                        :state (r/cursor state [:values :captcha])
                        :error (:captcha @errors)
                        :on-focus (partial on-focus :captcha)
                        :disabled @disabled?
                        :maxlength 2}]]
            
            [:div.form__button-wrapper
             [:button.form__button
              {:type "button"
               :disabled @disabled?
               :on-click
               (fn [evt]
                 (let [values (->> (:values @state)
                                   (reduce-kv
                                    (fn [m k v]
                                      (assoc m k (clojure.string/trim v)))
                                    {}))]
                   (binding [des/*captcha* (:captcha @state)]
                     (if-let [errors (not-empty (first (apply b/validate values valid-contact)))]
                       (swap! state assoc :errors errors)
                       (do
                         (when on-submit (on-submit values))
                         (swap! state assoc
                                :step :sending
                                :disabled? true
                                :errors {})
                         
                         (js/setTimeout
                          (fn []
                            (swap! state assoc :step :sent)
                            (js/setTimeout on-close 1500))
                          2000))))))}
              
              [:span.form__text-wrapper (case (:step @state)
                                          :sending
                                          [:div.three-dots
                                           [:span.dot][:span.dot][:span.dot]]
                                          
                                          "Send it") ]]]]]))])))




#_(defcard-rg contact
  [contact-form])





(defcard-rg nightmode
  [:button {:on-click toggle-theme!}
   "Nightmode"])



#_(defn slider []
  (let [siema-obj (atom nil)
        this (r/current-component)]
    (r/create-class
     {:display-name "proto.slider"
      :component-did-mount
      (fn [this]
        (reset! siema-obj (js/Siema.)))

      :component-will-unmount
      (fn [this]
        (.. @siema-obj destroy))

      :reagent-render
      (fn []
        ^{:key "proto.slider"}
        (into
         [:div.siema (r/props this)]
         (r/children this)))})))


#_(defcard-rg slider-experiments
  [des/slider
   [:div
    [:a {:href "http://google.com"} "span1"]
    [:a "span2"]]
   [:div
    [:a "span3"]
    [:a "span4"]]])




       
(def loading-icon (u/preload-resource "public/img/loading.svg"))
(def search-icon (u/preload-resource "public/img/search.svg"))

(defn end-anim [el & [run-fn]]
  (d/listen-once!
   el "animationiteration"
   #(do (d/set-style! el "animation-play-state" "paused")
        (when run-fn (run-fn)))))

(defn start-anim [el]
  (d/set-style! el "animation-play-state" "running"))

(def anim-sel "#loading-icon #circle-small")



(defn search-animation []
  [:div
   [:div {:style {:margin-bottom "1em"}}
    [:button {:on-click
              #(-> (d/sel1 "#loading-icon")
                   (d/toggle-class! "freeze"))}
     "Pause"]]
   
   [:div.descryptor.search-card
    [:div (inline-html loading-icon)]]])



(defcard-rg search-animation
  [search-animation])






(defcard-rg searching
  [search-box {:tags ["hello" "friend"]}])










;; add position balancing to add-spec
;; apply-view inside charting fn
;; use original lin-tick-marks
;; use thi.ng :range to filter values instead of apply-view
;; :xdomain [(- xend period) xend]



;; add-spec -> add-ticks


#_(defonce coins-price (u/preload-edn "proto-coins.edn"))






(comment
  (-> coins
      first
      :data :price
      (pcu/add-spec {:view :1d})
      (line/add-ticks {:view :1d :size :default})
      :spec)




  (-> coins
      first
      :data :price :1d
      :spec :xdomain
      (line/lin-tick-marks (:default (:major (:1d cd/xgrid-spec))))
      (->> (map (comp (partial cf/unparse {:format-str "hh:mm"}) ctc/from-long))))



  ;; yticks
  (-> coins
      first
      :data :price :1m
      :spec :ydomain
      ((juxt identity (fn [[ystart yend]] (/ (- yend ystart) 4))))
      ((fn [[domain delta]] (line/lin-tick-marks domain delta))))


  (line/lin-tick-marks [9525.18730317987 10101.5015231817] 2020.30030463634)

  )


(def coin-price (-> coins (nth 4) :data :price))

(def minmax (->> (:data coin-price) (map first) pcu/min-max))



(->> (pcu/filter-period [(- (second minmax) (->> :1w (get cd/xgrid-spec) :period))
                        (second minmax)]
                       (:data coin-price))
     
     (map first) pcu/min-max pcu/get-size)

(->> :1w (get cd/xgrid-spec) :period)

(->> :1w (get cd/xgrid-spec) :period)





(defcard-rg chart-experiments
  [price-chart
   coin-price
   {:period-selectors? true
    :dynamic? true
    :xticks? true
    :yticks? true
    :view :6m
    :spec {:size [600 140]
           :line-width "2px"
           :grid-width "0.9px"}}])









(defcard-rg placeholder-experiments
  [chart-placeholder {:size [24 7]}])






(def grid-spec {:1y [3 12]
                :6m [3 6]
                :1m [3 6]
                :1w [3 7]
                :all [3 1]})






(defn grid-render [& [{:keys [el-size grid-size]}]]
  (if-not (and el-size grid-size)
    [:svg {:style {:position :absolute :left 0}}]
    
    (let [[rows columns] grid-size
          [width height] el-size
          xstep (m/roundto (/ width columns) 1)
          ystep (m/roundto (/ height rows) 1)
          width (inc (* xstep columns))
          height (inc (* ystep rows))]

      [:svg {:width width :height height
             :style {:position :absolute :left 0}
             :xlmns "http://www.w3.org/2000/svg"}
     
       [:defs
        [:pattern#grid {:width xstep :height ystep
                        :patternUnits "userSpaceOnUse"
                        :shape-rendering "crispEdges"}
       
         [:path {:d (str "M " xstep " 0 L 0 0 0 " ystep)
                 :fill :none
                 :stroke "black"
                 :stroke-width "0.25"}]]]
     
       [:rect {:width "100%" :height "100%" :fill "url(#grid)"}]])))




(defn grid [[rows columns]]
  (r/with-let [rect (r/atom nil)
               el-key (gensym)
               this (r/current-component)
     
               get-size! #(some->>
                           (rd/dom-node this)
                           (d/parent)
                           (d/bounding-client-rect)
                           (reset! rect))]

    [window-event-listener
     {:on-resize get-size!
      :on-mount get-size!}
     
     (let [{:keys [width height]} @rect]
       [grid-render {:grid-size [rows columns]
                     :el-size [width height]}])]))





(defcard-rg grid-experiments
  [:div {:style {:position :relative
                 :width "100%"
                 :height "200px"
                 :overflow :visible}}
   [grid [3 12]]])






(defn ^:export init! []
  (devcards.core/start-devcard-ui!))


(init!)














(comment
  
  (def grid-svg "<svg width=\"%d\" height=\"%d\" xlmns=\"http://www.w3.org/2000/svg\" shape-rendering=\"crispEdges\" style=\"padding:0px;\"><defs><pattern width=\"%d\" height=\"%d\" patternUnits=\"userSpaceOnUse\" id=\"grid\"><path d=\"M %d 0 L 0 0 0 %d\" fill=\"none\" stroke=\"#eaf0f2\" stroke-width=\"%f\"></path></pattern></defs><rect width=\"100%\" height=\"100%\" fill=\"url(#grid)\"></rect></svg>")




  (defn grid2 [[rows columns]]
    (let [rect (r/atom nil)]
      (r/create-class
       {:component-did-mount
        (fn [this]
          (->> (rd/dom-node this)
               (d/bounding-client-rect)
               (reset! rect)))


        :reagent-render
        (fn [[rows columns]]
          (if @rect
            [:div
             {:style {:position :absolute
                      :width "100%"
                      :height "100%"
                      :padding 0 :margin 0 :border 0
                      :background
                      (let [{:keys [width height]} @rect
                            xstep (m/roundto (/ width columns) 1)
                            ystep (m/roundto (/ height rows) 1)
                            fragment (->> (format grid-svg
                                                  (inc (* xstep columns))
                                                  (inc (* ystep rows))
                                                  xstep ystep xstep ystep 0.5)
                                          (format "url(data:image/svg+xml;utf8, %s) no-repeat 100%"))]
                        (info fragment)
                        fragment)}}]
          
            [:div
             {:style (cond-> {:position :absolute
                              :width "100%"
                              :height "100%"
                              :padding 0 :margin 0 :border 0})}]))})))


  ) ;;comment












(comment

  (let [data (->> (first coins)
                  :data :github :day :data)
        data (cons (first data) (drop 4 data))
        [from to] (viz/value-domain-bounds (map first data))
        commits (matrix/commits-per-day from data)
        rows 7
        cols (-> (last commits) first (/ rows) Math/ceil)
        mat (->> (repeat (* rows cols) 0)
                 (into [])
                 (transient))]
    
    (doseq [[d n] commits]
      (assoc! mat d n))
    
    {:spec {:rows rows
            :cols cols}
     :data (persistent! mat)})


  )
