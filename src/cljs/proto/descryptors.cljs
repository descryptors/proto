(ns proto.descryptors
  (:require [reagent.core :as r]
            [reagent.dom :as rd]
            [dommy.core   :as d]
            [linked.core  :as linked]
            [goog.string  :as gstring]
            [goog.string.format]
            [goog.dom :as dom]
            [thi.ng.math.core :as m]
            [taoensso.timbre :refer [info]]
            [bouncer.core :as b]
            [bouncer.validators :as bv]
            [clojure.edn :refer [read-string]]
            [hiccups.runtime]
            [cljsjs.clipboard]
            [proto.mixins :refer [window-event-listener]]
            [proto.util :as u :refer [inline-html coll->pattern
                                      expand-descendants]]
            [proto.charts.util   :as cu :refer [export-viz]]
            [proto.charts.matrix :refer [matrix-chart]]
            [proto.charts.bar    :refer [bar-chart]]
            [proto.charts.line   :as cl :refer [line-chart]]
            [proto.charts.defaults :as cd :refer [xgrid-spec ygrid-spec]]
            [proto.dropdown      :refer [dropdown]]
            [proto.descryptors.defaults :as defaults]
            [proto.toolbar :refer [label button radio-button toggler
                                   info-icon theme-icon
                                   vertical-break
                                   diagonal-break
                                   horizontal-break]])

  (:require-macros [hiccups.core :as hiccups]))





(def search-modes
  {:union {:on-click identity
           :icon [:div.line0 (inline-html defaults/svg-union)]}
   :intersection {:on-click identity
                  :icon [:div.line0 (inline-html defaults/svg-intersection)]}})




(defn editable [{:keys [state on-change tag] :as props}]
  (r/create-class
   {:component-did-update
    (fn [this _]
      (.setCustomValidity (rd/dom-node this)
                          (str (:error (r/props this)))))
    
    :reagent-render
    (fn [{:as props :keys [state error on-change tag]
          :or {tag :input}}]
      [tag
       (-> props
           (dissoc :state :error :tag)
           (merge {:value @state
                   :on-change #(cond-> (reset! state (.. % -target -value))
                                 on-change on-change)}))])}))




;; CSS animation experiments

(comment
  (defn pause-anim [el]
    (d/listen-once!
     el "animationiteration"
     #(d/set-style! el "animation-play-state" "paused")))

  (defn start-anim [el]
    (d/set-style! el "animation-play-state" "running"))

  (def anim-sel "#loading-icon #circle-small")

  (defn one-loop []
    (start-anim (d/sel1 anim-sel))
    (pause-anim (d/sel1 anim-sel))))




(defn search-toolbar
  [& [{:as toolbar-opts :keys [search-modes-opts search-mode]
       :or {search-mode :intersection}}]]

  (r/with-let [current-search-mode (r/atom search-mode)]
    [:div.search-toolbar
     (let [{:keys [on-click icon]}
           (get search-modes-opts @current-search-mode)]
       
       [:div.search-toolbar__item.clickable
        [toggler {:post-click on-click
                  :on-click #(swap! current-search-mode
                                    (fn [csm]
                                      (-> (dissoc search-modes-opts csm)
                                          keys first)))}
         icon]
        
        [:div.overlay__text.overlay__text--union]])]))



(defn search-tag [opts tag]
  [:span.search-tag opts
   (->> tag (str (when (not= (first tag) "~") "#")))])



(defn search-box
  [{:as opts :keys [add-tag remove-tag tags toolbar-opts
                    state search-icon props]}]

  (let [local-text (r/atom nil)
        local-tags (or state (r/atom (into (linked/set) tags)))
        run-on-update (r/atom [])
        
        remove-tag* (fn [idx]
                      (swap! local-tags (fn [tags] (disj tags (nth tags idx))))
                      (when remove-tag
                        (swap! run-on-update conj #(remove-tag idx))))
        
        add-tag* (fn []
                   (let [text @local-text]
                     (when-not (empty? text)
                       (when-not (get @local-tags text)
                         (swap! local-tags conj text)
                         (when add-tag
                           (swap! run-on-update conj #(add-tag text))))
                       (reset! local-text nil))))

        focus-input #(some-> (d/sel1 ".search-input-field") .focus)]

    (r/create-class
     {:component-did-update
      (fn [_]
        (swap! run-on-update
               (fn [run-fns]
                 (doseq [f run-fns] (when f (f)))
                 (empty run-fns))))

      
      :reagent-render
      (fn [{:keys [search-icon props toolbar-opts]}]
        
        [:div.descryptor.search-card
         ;; DOM opts
         (merge {:class (str (:class opts))
                 :on-click focus-input}
                props)
         
         [:div.search-card__input-box
          
          ;; search input
          ;;
          [editable
           {:state local-text
            :auto-focus true
            :class "search-input-field"
            :placeholder "search"
            :on-key-down
            (fn [e]
              (condp = (.. e -key)
                " "  (do (add-tag*)
                         (.preventDefault e))
                                         
                "Enter" (add-tag*)
                                         
                "Backspace" (when (and (empty? @local-text) (< 0 (count @local-tags)))
                              (let [idx (dec (count @local-tags))]
                                (remove-tag* idx)))
                false))}]]

         
         ;; display tags
         ;;
         (when-not (empty? @local-tags)
           [:div.search-card__tag-box
            (reverse
             (map-indexed
              (fn [idx tag]
                [search-tag {:on-click #(remove-tag* idx)
                             :key idx} tag])
              @local-tags))

            ;; show clear all icon when at least 2 tags are visible
            ;;
            (when (< 1 (count @local-tags))
              [:span.search-card__clear {:on-click #(swap! local-tags empty)}
               [:span.search-card__clear-icon
                (inline-html defaults/svg-arrow-up)]
               [:span.search-card__clear-text "clear all"]])])


         ;; show toolbar when at least 2 tags are visible
         ;;
         (when (< 1 (count @local-tags))
           [:div.search-card__toolbar-box
            [search-toolbar
             (merge {:search-modes-opts search-modes
                     :search-mode :intersection}
                    toolbar-opts)]])

         search-icon])})))





(defn get-width [selector]
  (some-> (d/sel1 selector)
          (d/bounding-client-rect)
          :width))



(defn get-first-card-width []
  (some-> (d/sel1 ".descryptors-grid :first-child")
          (d/bounding-client-rect)
          :width))



(defn amount-of-cards-in-rows
  "Returns the number of cards that will fit in the grid with
  specified number of rows."
  [rows-number]
  (let [grid-width (get-width :.descryptors-grid)
        card-width (or (get-width :.search-card)
                       (get-first-card-width))]
    
    (if (and card-width (< 0 card-width))
      (when (and grid-width (< 0 grid-width))
        (max rows-number (dec (* rows-number (int (/ grid-width card-width))))))
      rows-number)))




(defn toggle-theme! []
  (d/toggle-class! js/document.documentElement :night-mode-on))



(defn transition-theme! [prev curr]
  (when prev
    (some->> (get defaults/theme-classes prev)
             (d/remove-class! js/document.documentElement)))
  (when curr
    (some->> (get defaults/theme-classes curr)
             (d/set-class! js/document.documentElement))))



(defn toggle-info! []
  (let [on? (not (d/has-class? js/document.documentElement :overlay-on))
        toggle-fn (if on? d/add-class! d/remove-class!)]
    ;; toggle overlay
    (toggle-fn js/document.documentElement :overlay-on)))



#_(defn toggle-search! [on?]
  (let [toggle-fn (if on? d/add-class! d/remove-class!)]
    (some-> (d/sel1 ".search-card")
            (toggle-fn "mobile-visible"))))




;; Toolbar configuration
;;

(def chart-links [[:price-chart  "Price"]
                  [:github-chart "Github"]])

(def sort-items [[:marketcap-index "Marketcap"]
                 [:github-index "Github"]
                 #_[:price-index  "Price"]])
(def sort-texts (into {} sort-items))

(def theme-items [[:light-theme "Light"]
                  [:dark-theme  "Dark"]])
(def theme-texts (into {} theme-items))


(derive :toolbar/info     :toolbar/all)
(derive :toolbar/theme    :toolbar/all)
(derive :toolbar/charts   :toolbar/all)
(derive :toolbar/sort-idx :toolbar/all)

(derive :toolbar/rows-per-page :footer-toolbar/all)


(defn toolbar
  [{:as opts :keys [state on-change show]
    :or {show #{:toolbar/all} on-change identity}}]
  
  (let [show (expand-descendants show)]
    (let [items
          (->>
           ;; display chart selection radio buttons
           [(when (show :toolbar/charts)
              [:div.toolbar__filter-wrapper.toolbar__item
               [:div.toolbar__filter-container
                [label "Charts:"]
                (for [[k l] chart-links]
                  ^{:key l}
                  [radio-button {:label l
                                 :on-click #(on-change :toolbar/charts k)
                                 :on? (get-in state [:toolbar/charts k])}])]])
           

            ;; display sorting dropdown
            (when (show :toolbar/sort-idx)
              [:div.dropdown-wrapper.toolbar__item
               [label "Sort by:"]
               [dropdown
                {:text (get sort-texts (:toolbar/sort-idx state))
                 :container-class "dropdown dropdown--toolbar"
                 :content (for [[k text] sort-items]
                            {:on-click #(on-change :toolbar/sort-idx k)
                             :key text
                             :text text
                             :value k})}]])

           
            ;; display theme icon
            (when (show :toolbar/theme)
              [theme-icon {:post-click #(on-change :toolbar/theme)}])


            ;; display info icon
            (when (show :toolbar/info)
              [info-icon
               {:on? (:toolbar/info state)
                :post-click #(on-change :toolbar/info)}])


            ;; display rows per page dropdown
            (when (show :toolbar/rows-per-page)
              [:div.dropdown-wrapper.toolbar__item
               [label "Rows per page:"]
               [dropdown
                {:text (str (:toolbar/rows-per-page state))
                 :container-class "dropdown dropdown--toolbar"
                 :content
                 (for [row-count (remove #(= % (:toolbar/rows-per-page state)) [5 10 20])]
                   {:on-click #(on-change :toolbar/rows-per-page row-count)
                    :key row-count
                    :text (str row-count)
                    :value row-count})}]])]

           (remove nil?))]

      
      (when-not (empty? items)
        [:div {:key (:class opts)
               :class (:class opts)}
         (into [:<>] (interpose [diagonal-break] items))]))))




(derive :toolbar/info     :mobile-toolbar/all)
(derive :toolbar/theme    :mobile-toolbar/all)
(derive :toolbar/charts   :mobile-toolbar/all)
(derive :toolbar/search   :mobile-toolbar/all)
(derive :toolbar/sort-idx :mobile-toolbar/all)


(defn mobile-toolbar [{:as opts :keys [show on-change active-icon]
                       :or {show #{:mobile-toolbar/all} on-change identity}}]
  
  (r/with-let [active? #(when (= %1 %2) " active")]
    
    (let [show (expand-descendants show)]
      
      (some->>
       [ ;; display charts selection
        (when (show :toolbar/charts)
          [:div.mobile-icon-wrapper
           [:div (merge {:class (str "mobile-chart-icon"
                                     (active? :toolbar/charts active-icon))
                         :on-click #(on-change :toolbar/charts)}
                        (inline-html defaults/svg-mobile-charts))]])
            

        ;; display sorting dropdown
        (when (show :toolbar/sort-idx)
          [:div.mobile-icon-wrapper
           [:div (merge {:class (str "mobile-sort-icon"
                                     (active? :toolbar/sort-idx active-icon))
                         :on-click #(on-change :toolbar/sort-idx)}
                        (inline-html defaults/svg-mobile-sort))]])


        ;; display theme icon
        (when (show :toolbar/theme)
          [:div.mobile-icon-wrapper
           [:div (merge {:class "mobile-theme-icon"
                         :on-click #(on-change :toolbar/theme)}
                        (inline-html defaults/svg-mobile-theme))]])

            
        ;; display info icon
        (when (show :toolbar/info)
          [:div.mobile-icon-wrapper
           [:div (merge {:class "mobile-info-icon"
                         :on-click #(on-change :toolbar/info)}
                        (inline-html defaults/svg-mobile-info))]])

            
        ;; toggle search box display (during mobile)
        (when (show :toolbar/search)
          [:div.mobile-icon-wrapper
           [:div (merge {:class (str "mobile-search-icon"
                                     (active? :toolbar/search active-icon))
                         :on-click #(on-change :toolbar/search)}
                        (inline-html defaults/svg-mobile-search))]])]
           
       (remove nil?)
       (not-empty)
       (into [:<>])
       (conj [:div {:key (:class opts)
                    :class (:class opts)}])))))




;; Sticky Tracking
;;

(defn sticky-checker [ratom pos]
  (fn []
    (if (< pos window.pageYOffset)
      (when (empty? @ratom)
        (reset! ratom " animation sticky "))
      (when (not-empty @ratom)
        (reset! ratom "")))))


(defonce throttle200 (u/throttler 200))
(defonce sticky-class (r/atom ""))
(def sticky-check-fn (sticky-checker sticky-class 420))
(def check-sticky #(throttle200 sticky-check-fn))



(defn sticky-mobile-toolbar [opts]
  [mobile-toolbar (update opts :class str @sticky-class)])



(defn sticky-header [& children]
  (r/create-class
   {:component-will-unmount
    (fn [_]
      (swap! sticky-class #(when (not-empty %) " sticky ")))
    
    :reagent-render
    (fn [& children]
      [window-event-listener
       {:on-scroll check-sticky}
       
       (into
        [:div.header {:class @sticky-class}]
        children)])}))





;; Charts
;;


(defn static-chart [props child]
  [:div.chart props child])




(defn dynamic-chart
  [props chart-data
   {:as opts :keys [xticks? yticks? view-fn placeholder]}]
  
  (r/with-let [this (r/current-component)
               dom-node  (atom nil)
               dom-size  (r/atom nil)
               debounce  (u/debouncer 200)
               get-size! (fn [dom]
                           (let [chart-data (first (r/children this))
                                 [width height] (:size (:spec chart-data))
                                 hw (/ height width)]
                             (->> (d/bounding-client-rect dom)
                                  :width
                                  ((juxt identity (partial * hw)))
                                  (reset! dom-size))))]
    
    [window-event-listener
     {:on-resize (fn [e]
                   (when @dom-node
                     (debounce #(get-size! @dom-node))))
      :on-mount  (fn [this]
                   (some->> (rd/dom-node this)
                            (reset! dom-node)
                            get-size!))}

     [:div.chart props
      (if @dom-size
        (let [[width height :as size] @dom-size]
          [:div {:dangerouslySetInnerHTML
                 {:__html (-> chart-data
                              (cl/add-ticks
                               (assoc opts :screen
                                      (cond
                                        (< 400 width 600) :sm
                                        (<= width 400) :xs
                                        :else :desktop)))
                              (assoc-in [:spec :size] size)
                              (view-fn) :data hiccups/html)}}])
        placeholder)]]))




(def view-data-lookup
  {:1d [:minute :hour :day]
   :1w [:hour :day :minute]
   :1m [:hour :day]
   :6m [:day :hour]
   :1y [:day :hour]})



(def tooltip-lookup
  {:1d "1 day"
   :1w "1 week"
   :1m "1 month"
   :6m "6 months"
   :1y "1 year"})



(defn descryptor-chart
  [chart-data opts]
  (let [mounted (r/atom false)
        chart-class (str (:class opts))
        view (r/atom (or (:view opts) defaults/chart-period))]

    (r/create-class
     {:component-did-mount
      (fn [_]
        (reset! mounted true))
      
      :reagent-render
      (fn [chart-data {:keys [dynamic? spec period-selectors] :as opts}]
        (let [view-svg (or (get-in chart-data [:svg @view])
                           (get-in chart-data [@view :svg]))
              
              size (or (:size spec)
                       (get-in view-svg [:spec :size])
                       defaults/chart-size)

              ;; check if we have :view data otherwise use top level
              ;; data
              view-data (or (some chart-data (get view-data-lookup @view))
                            #_(when (get-in chart-data [@view :data])
                                (get chart-data @view))
                            chart-data)
              
              view-data (->> (assoc spec :size size)
                             (update view-data :spec merge))

              empty-space (export-viz {:size size})
              placeholder [static-chart {:class "chart--placeholder"} empty-space]]

          [:div.chart-wrapper
           
           (when (and period-selectors dynamic?)
             [:div.period-wrapper
              {:style {:opacity (if (and @mounted (:data view-data)) 1 0)}}
              (->> period-selectors
                   (map (fn [[text k]]
                          ^{:key k}
                          [:a (cond-> {:on-click #(reset! view k)}
                                (= k @view) (-> (assoc-in [:style :font-weight] :bold)
                                                (update :class str "active")))
                           text]))
                   doall)])

           
           (if @mounted
             (cond
               ;; static svg chart
               ;;
               (and (:data view-svg) (not dynamic?))
               [static-chart {:class chart-class
                              :title (get tooltip-lookup @view)
                              :dangerouslySetInnerHTML
                              {:__html (:data view-svg)}}]

               ;; dynamic chart
               ;;
               (:data view-data)
               [dynamic-chart {:class chart-class}
                (-> view-data
                    (cu/add-spec {:view @view :size size})
                    #_(update :data (partial cu/period-precision
                                             (get cd/precisions @view defaults/chart-period))))
                (assoc opts :placeholder empty-space)]
               
               :else placeholder)

             placeholder)]))})))




(defn github-chart
  [github-data & [{:keys [period-selectors?] :as opts}]]
  
  [descryptor-chart github-data
   (cond-> opts     
     period-selectors? (assoc :period-selectors
                              [["6m" :6m]
                               ["1y" :1y]])

     :default (assoc :class "chart--github"
                     :view-fn matrix-chart))])




(defn price-chart
  [price-data & [{:keys [period-selectors? spec] :as opts}]]
  
  [descryptor-chart price-data
   (cond-> opts
     period-selectors? (assoc :period-selectors
                              [["1d"   :1d]  ["1w"   :1w]
                               ["1m"   :1m]  ["6m"   :6m]
                               ["1y"   :1y]])
     
     :default (assoc :class "chart--price"
                     :view-fn line-chart))])





(defn highlighter []
  (let [we-had-tags? (atom false)]
    (fn [tags]
      (let [searchables (d/sel js/document.documentElement :.sbl)
            pattern (coll->pattern tags)]
        (if (empty? tags)
          ;; reset all searchables to initial color
          (when @we-had-tags?
            (doseq [el searchables]
              (d/remove-class! el :highlight))
            (reset! we-had-tags? false))
            
          (doseq [el searchables]
            (if (re-find pattern (d/text el))
              (d/add-class! el :highlight)
              (d/remove-class! el :highlight))
            (reset! we-had-tags? true)))))))






;; fixme: textarea redrawn too many times on edit.
(defn overlay-scrollbars [props content]
  (let [this (r/current-component)
        dom-size (atom nil)
        get-size (fn [this]
                   (->> (rd/dom-node this)
                        ((juxt #(.-scrollWidth %)
                               #(.-scrollHeight %)))))
        instance (atom nil)
        init-scroll #(reset! instance
                             (->> (clj->js {"scrollbars" {"autoHide" "leave"}})
                                  (js/OverlayScrollbars (rd/dom-node %))))]
    (r/create-class
     {:component-did-mount
      (fn [this]
        (reset! dom-size (get-size this))
        (init-scroll this))

      :UNSAFE-component-will-update
      (fn [this _]
        (when (:dynamic? (r/props this))
          (when-not (= @dom-size (get-size this))
            (swap! instance (fn [inst] (when inst
                                         (.destroy inst)
                                         nil))))))
      
      :component-did-update
      (fn [this _]
        (when (:dynamic? (r/props this))
          (let [new-size (get-size this)]
            (when-not (= @dom-size new-size)
              (reset! dom-size new-size)
              (init-scroll this)))))

      :reagent-render
      (fn []
        (first (r/children this)))})))





(def ^:dynamic *captcha* nil)


;; Bouncer validator config
;;

(bv/defvalidator validate-captcha
  {:default-message-format "invalid captcha"}
  [captcha]
  (= (:answer *captcha*) (read-string captcha)))



(def valid-contact
  [:email [[bv/required]
           [bv/email]]
   :message bv/required
   :captcha [[bv/required]
             [validate-captcha]]])




(defn gen-captcha []
  (let [a (inc (rand-int 9))
        b (inc (rand-int 9))
        answer (+ a b)]
    {:answer answer
     :question (str "What is " a " + " b)}))



(defn update-contact-message-error [error]
  (when-let [textarea (d/sel1 "div.form__textarea")]
    (let [update-fn (if (not-empty error) d/add-class! d/remove-class!)]
      (update-fn textarea "error"))))



(defn contact-form [{:as props :keys [on-close on-submit]}]
  (let [state (r/atom {:disabled? false
                       :step :send
                       :errors {}
                       :captcha (gen-captcha)})
        disabled? (r/cursor state [:disabled?])
        errors    (r/cursor state [:errors])
        ;; clear errors on focus
        on-focus  (fn [id evt] (swap! state update :errors dissoc id))]
    
    (fn []
      [:div.form__wrapper
       ;; close button
       [:div.form__close
        (merge {:on-click on-close}
               (inline-html defaults/svg-close))]

       
       ;; form input items
       (into
        [:form.form {:class (str "form--" (name (:step @state)))}]
        
        (case (:step @state)
          :sent
          [[:div.sent]]
          
          [[:div.form__group.form__group--half
            [:div.form__first
             [editable {:type "text"
                        :class "form__input"
                        :state (r/cursor state [:values :first-name])
                        :auto-focus true
                        :disabled @disabled?
                        :id "first_name"
                        :name "user_name"
                        :placeholder "First Name"}]]
            
            [:div.form__last
             [editable {:type "text"
                        :class "form__input"
                        :state (r/cursor state [:values :last-name])
                        :disabled @disabled?
                        :id "last_name"
                        :name "user_last"
                        :placeholder "Last Name"}]]]

           [:div.form__item.form__email
            [editable {:type "email"
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
                       :class "form__input"
                       :state (r/cursor state [:values :company])
                       :disabled @disabled?
                       :id "company"
                       :name "user_company"
                       :placeholder "Company"}]]
           
           [:div.form__item.form__message
            [overlay-scrollbars
             [editable {:id "msg"
                        :tag :textarea
                        :class "form__textarea"
                        :state (r/cursor state [:values :message])
                        :error (:message @errors)
                        :on-focus (fn [evt]
                                    (on-focus :message evt)
                                    (update-contact-message-error nil))
                        :disabled @disabled?
                        :name "user_message"
                        :placeholder "Message"}]]]

           [:div.form__group.form__group--last
            [:div.form__capcha-wrapper
             [:label.form__label.form__label--capcha {:for "capcha"}
              (:question (:captcha @state))]
             [editable {:id "capcha"
                        :class "form__input form__input--capcha"
                        :state (r/cursor state [:values :captcha])
                        :error (:captcha @errors)
                        :on-focus (partial on-focus :captcha)
                        :disabled @disabled?
                        :maxLength 2}]]
          
            [:div.form__button-wrapper
             [:button.form__button
              {:type "button"
               :disabled @disabled?
               :on-click
               (fn [evt]
                 (let [ ;; remove trailing whitespace from input data
                       values (->> (:values @state)
                                   (reduce-kv
                                    (fn [m k v]
                                      (assoc m k (clojure.string/trim v)))
                                    {}))]
                   (binding [*captcha* (:captcha @state)]
                     ;; b/validate -> [{:input1 errors1 ...} {detailed info...}]
                     (if-let [errors (not-empty (first (apply b/validate values valid-contact)))]
                       (do (swap! state assoc :errors errors)
                           ;; manually update duplicated textarea div
                           (update-contact-message-error (:message errors)))
                       (do
                         (when on-submit (on-submit (dissoc values :captcha)))
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





(defn about [{:keys [on-close on-contact-click]
              :or {on-contact-click identity on-close identity}}]
  [window-event-listener {:on-mount #(window.scroll 0 0)}
   [:div.about__wrapper
    [:div.about__close
     (merge {:on-click on-close}
            (inline-html defaults/svg-close))]
   
    [:div.about
     [:div.about__logo
      [:a (merge {:on-click on-close}
                 (inline-html defaults/svg-logo))]]
    
     [:p.about__text
      "We are on our way to building a clean and intuitive interface for searching and discovering blockchain projects. Join our Reddit " [:a {:href "https://reddit.com/r/descryptors" :target :blank} "community"] " and follow the journey."]

     [:p.about__text
      "Thanks for dropping by!"]

     [:p.about__text
      [:a {:target :self
           :on-click (comp on-close on-contact-click)}
       "Contact us."]]]]])




(defn cookie-notice [{:keys [on-close terms-opts]
                      :or {on-close identity}}]
  [:div.notice__wrapper
   [:div.notice__close
    (merge {:on-click on-close}
           (inline-html defaults/svg-close-thin))]
   
   [:div.notice__content
    [:p.notice__text
     "This site uses cookies. By continuing to use this site, you agree with our "
     [:a.notice__link terms-opts
      #_{:target :self
         :on-click on-terms-click}
      "terms and conditions"] "."]]])





(defn terms-and-conditions [{:keys [on-close on-contact-click]
                             :or {on-close identity on-contact-click identity}}]
  [window-event-listener {:on-mount #(window.scroll 0 0)}
   [:div.about__wrapper
    [:div.about__close
     (merge {:on-click on-close}
            (inline-html defaults/svg-close))]
   
    [:div.about
     [:div.about__logo
      [:a (merge {:on-click on-close}
                 (inline-html defaults/svg-logo))]]
     
     [:h1.about__title
      "Terms and Conditions"]

     [:h2.about__subtitle "General"]
     
     [:p.about__text
      "Just like most websites, Descryptors uses cookies, which give us the opportunity to render services to our users.  In this document, we explain how we use cookies, and what users can do about it (if they want)."]

     [:h2.about__subtitle "Which cookies do we use and why?"]
     
     [:p.about__text
      "We use cookies to store the options you have selected."]

     [:h2.about__subtitle "What should you do if you don't want to use cookies?"]
     
     [:p.about__text
      "You can disable them using your browser’s security settings. An important note: the settings should be applied to all browsers you use (both on your computer and smartphone). If you choose to disable cookies, please note that Descryptors may operate unpredictably."]

     [:h2.about__subtitle "How do you learn more?"]
     
     [:p.about__text
      [:a {:target :self
           :on-click (comp on-close on-contact-click)}
       "Write us a message."]]]]])





(def libs [["Clojure" "https://clojure.org"]
           ["ClojureScript" "https://clojurescript.org"]
           ["Reagent" "https://github.com/reagent-project/reagent"]
           ["Re-Frame" "https://github.com/day8/re-frame"]
           ["thi.ng" "http://thi.ng/"]
           ["Reitit" "https://github.com/metosin/reitit"]
           ["http-kit" "https://github.com/http-kit/http-kit"]
           ["Integrant" "https://github.com/weavejester/integrant"]
           ["Sente" "https://github.com/ptaoussanis/sente"]
           ["Carmine" "https://github.com/ptaoussanis/carmine"]
           ["Timbre" "https://github.com/ptaoussanis/timbre"]
           ["Figwheel" "https://figwheel.org/"]
           ["nREPL" "https://github.com/nrepl/nrepl"]
           ["Roll" "https://github.com/dimovich/roll"]])



(defn code-page [{:keys [on-close] :or {on-close identity}}]
  [window-event-listener {:on-mount #(window.scroll 0 0)}
   [:div.about__wrapper
    [:div.about__close
     (merge {:on-click on-close}
            (inline-html defaults/svg-close))]
   
    [:div.about
     [:div.about__logo
      [:a (merge {:on-click on-close}
                 (inline-html defaults/svg-logo))]]
     
     [:p.about__text
      "This project uses:"]

     [:p.about__text {:style {:line-height "150%"}}
      (into [:<>] (interpose
                   (gstring/unescapeEntities "  &#8226;  ")
                   (for [[project link] libs]
                     [:a {:href link :target :blank} project])))]]]])






(defn clipboard
  "[clipboard [:a {:data-clipboard-text ...}]]"
  [[tag opts content]]
  (let [clipboard-atom (atom nil)]
    (r/create-class
     {:display-name "clipboard"
      :component-did-mount
      #(let [clipboard (new js/ClipboardJS (rd/dom-node %))]
         (reset! clipboard-atom clipboard))
      
      :component-will-unmount
      #(when-not (nil? @clipboard-atom)
         (.destroy @clipboard-atom)
         (reset! clipboard-atom nil))
      
      :reagent-render
      (fn []
        [tag (update opts :on-click
                     (fn [old-fn]
                       (fn [evt]
                         ;; trigger tooltip animation
                         (when-let [dom (.. evt -target)]
                           (d/remove-class! dom :active)
                           (js/setTimeout
                            #(d/add-class! dom :active)
                            50))
                         (when old-fn (old-fn evt)))))
         content])})))


