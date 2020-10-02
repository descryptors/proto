(ns ^:figwheel-hooks proto.figwheel
  (:require [dommy.core :as d :refer [sel1]]
            [taoensso.timbre :refer [info]]
            [linked.core :as linked]
            [cljs.reader]
            [reagent.core :as r]
            [reagent.dom :as rd]
            [proto.util :as u :refer [preload-edn inline-html]]
            [proto.mixins :as mixins]
            [proto.descryptors.defaults :as defaults]
            
            [proto.descryptors :as des
             :refer [toolbar mobile-toolbar search-box
                     highlighter amount-of-cards-in-rows
                     toggle-theme! toggle-info! contact-form about]]
            
            [proto.descryptors.common :as common
             :refer [descryptors-grid navigation logo
                     descryptor-single header footer]]))



;; preload sample coin data from project directory
(defonce coins (-> (->> (preload-edn "proto-coins.edn")
                        (map-indexed #(assoc %2 :coin-idx %1))
                        vec)
                   (update-in [0 :tags] (partial into ["eco"]))))

(defonce coins-in-grid (r/atom 20))
(defonce current-page  (r/atom :index))
(defonce current-coin-idx  (r/atom 10))


;; save which tags we clicked, keep track of that and
;; highlight them
(defonce highlight-tags (r/atom (into (linked/set) ["dapps" "currency" "what"])))
(defonce highlight-fn (highlighter))
(defonce highlight #(highlight-fn @highlight-tags))


(defonce app-db (r/atom {:show-contact false
                         :show-about   false
                         :show-notice  true
                         :index-scroll 0}))


;; initialize toolbar state with some defaults
(defonce toolbar-state
  (r/atom {:toolbar/charts defaults/visible-charts
           :toolbar/search false
           :toolbar/info false
           :toolbar/sort-idx :github-index
           :toolbar/theme :light-theme
           :toolbar/rows-per-page 5}))


(defonce mobile? (r/atom nil))

(defonce mobile-show (r/atom nil))
(defn toggle-mobile-show [path]
  (swap! mobile-show (fn [current new] (when-not (= current new) new)) path))



(defonce throttle300 (u/throttler 300))
(defonce on-grid-resize
  #(throttle300
    (fn []
      (when-let [new-step (some->> (:toolbar/rows-per-page @toolbar-state)
                                   (amount-of-cards-in-rows))]
        (cond->> new-step
          (and @mobile? (not= @mobile-show :toolbar/search))
          inc
          :default (reset! coins-in-grid)))
      
      (highlight))))




(defn descryptor-single* [props coin]
  (let [throttle (u/throttler 42)
        on-resize #(throttle js/alignDots)]
    (r/create-class
     {:reagent-render
      (fn [props coin]
        [mixins/window-event-listener
         {:on-resize on-resize
          :on-update js/alignDots
          :on-mount js/alignDots}
         
         [descryptor-single props coin]])

      :component-did-mount
      (fn [_]
        (window.scroll 0 0))})))




(defn descryptors-grid* [props coins]
  (r/create-class
   {:reagent-render
    (fn [props coins]
      ;; keep track of window size
      [mixins/window-event-listener
       {:on-resize on-grid-resize
        :on-mount  on-grid-resize
        :on-update highlight}
       
       ;; display descryptors
       [descryptors-grid props coins]])

    :component-did-mount
    (fn [_]
      (window.scroll 0 (:index-scroll @app-db))
      
      
      ;; we might need to inc card number when it's mobile and
      ;; search-card is not visible
      (add-watch mobile? :mobile
                 (fn [_ _ old new]
                   (when (not= old new)
                     (on-grid-resize)))))

    :component-will-unmount
    (fn [_]
      (swap! app-db assoc :index-scroll js/pageYOffset)
      (remove-watch mobile? :mobile?))}))





(defn handle-toolbar-change [& [path value event]]
  (condp = path
    :toolbar/charts
    (swap! toolbar-state update path #(u/toggle-set-item % value))

    :toolbar/theme
    (do (swap! toolbar-state update path not)
        (toggle-theme!))

    :toolbar/info
    (do (swap! toolbar-state update path not)
        (toggle-info!))

    :toolbar/rows-per-page
    (do (swap! toolbar-state assoc path value)
        (on-grid-resize))
    
    nil))



(defn handle-mobile-toolbar-change [& [path value event]]
  (condp = path
    :toolbar/charts
    (toggle-mobile-show :toolbar/charts)

    :toolbar/sort-idx
    (toggle-mobile-show :toolbar/sort-idx)
    
    :toolbar/theme
    (toggle-theme!)

    :toolbar/info
    (do (toggle-info!)
        (swap! toolbar-state update path not))

    :toolbar/search
    (do (if-let [bottom (some-> (d/sel1 :.search-card)
                                (d/bounding-client-rect)
                                :bottom)]
          ;; visible
          (if (neg? bottom)
            (window.scroll 0 0)
            (toggle-mobile-show :toolbar/search))
      
          ;; hidden
          (do
            (window.scroll 0 0)
            (toggle-mobile-show :toolbar/search)))

        (on-grid-resize))
    
    nil))



(defn index [coins]
  (let [wrapper-classes {:descryptor-single "wrapper--single"}
        ;; if we click on logo, go to main page
        logo-on-click #(when-not (= :index @current-page)
                         (reset! current-page :index))

        throttle (u/throttler 200)
        track-fn (u/mobile-tracker mobile? defaults/mobile-breakpoint)
        check-mobile #(throttle track-fn)        
        
        on-tag-click-conj
        #(let [tag (subs (d/text (.. % -target)) 1)]
           ;; toggle tag highlight
           (swap! highlight-tags
                  (fn [tags]
                    (if-not (get tags tag)
                      (conj tags tag)
                      tags)))
           (reset! current-page :index))


        ;; change coins on navigation
        single-navigation
        (fn []
          [navigation {:prev-opts
                       (if (< 0 @current-coin-idx)
                         {:on-click
                          #(swap! current-coin-idx
                                  (fn [idx] (max 0 (dec idx))))}
                         {:class "disabled"})

                       :next-opts
                       (if (< @current-coin-idx (dec (count coins)))
                         {:on-click
                          #(swap! current-coin-idx
                                  (fn [idx] (min (inc idx) (dec (count coins)))))}
                         {:class "disabled"})}])

        index-navigation (fn [] [navigation {:prev-opts {:class "disabled"}}])]


    (r/create-class
     {:component-did-mount
      (fn [_]
        (add-watch highlight-tags :tags
                   (fn [_ _ _ tags]
                     (highlight-fn tags))))

      :component-will-unmount
      (fn [_] (remove-watch highlight-tags :tags))


      :reagent-render
      (fn [coins]
        (let [cp @current-page]
          [mixins/window-event-listener
           {:on-resize check-mobile
            :on-mount check-mobile}

           (or
            ;; About
            
            (when (:show-about @app-db)
              [about {:on-close #(swap! app-db assoc :show-about false)
                      :on-contact-click #(swap! app-db assoc :show-contact true)}])


            ;; Terms and Conditions
            
            (when (:show-terms @app-db)
              [des/terms-and-conditions {:on-close #(swap! app-db assoc :show-terms false)
                                         :on-contact-click #(swap! app-db assoc :show-contact true)}])


            (when (:show-code @app-db)
              [des/code-page
               {:on-close #(swap! app-db assoc :show-code false)}])

            
            ;; Default view
           
            [:div.wrapper
             {:class (str (get wrapper-classes cp)
                          (when (:show-contact @app-db) " contact-visible"))}


             ;; OVERLAY
             ;;
             [:div.overlay
              {:on-click #(handle-toolbar-change :toolbar/info false)}
              (when @mobile?
                (->> (repeat 5 [:div.overlay__item])
                     (into [:div.overlay__mobile])))]

             
             ;; COOKIE NOTICE
             ;;
             (when (:show-notice @app-db)
               [des/cookie-notice {:terms-opts {:on-click #(swap! app-db assoc :show-terms true)}
                                   :on-close #(swap! app-db assoc :show-notice false)}])
             

             ;; HEADER
             ;;
             (if (nil? @mobile?)
               ;; we don't know if we're on mobile yet, so display only
               ;; the logo
               [common/header [logo {:on-click logo-on-click}]]

               (if @mobile?
                 ;;
                 ;; MOBILE HEADER
                 ;;
                 (case cp
                   
                   ;; INDEX
                   
                   :index
                   [:<>
                    [des/sticky-header
                     [logo {:on-click logo-on-click}]
                     (index-navigation)]
                    
                    [des/sticky-mobile-toolbar
                     {:class "toolbar--mobile"
                      :show #{:mobile-toolbar/all}
                      :active-icon @mobile-show
                      :on-change handle-mobile-toolbar-change}]
                    
                    [toolbar {:state @toolbar-state
                              :class "toolbar--mobile-grid"
                              :show #{@mobile-show}
                              :on-change handle-toolbar-change}]]
                   
                   
                   ;; SINGLE
                   
                   :descryptor-single
                   [common/header
                    [logo {:on-click logo-on-click}]
                    [mobile-toolbar {:class "toolbar--mobile toolbar--mobile-single"
                                     :show #{:toolbar/theme :toolbar/info}
                                     :active-icon @mobile-show
                                     :on-change handle-mobile-toolbar-change}]
                    (single-navigation)]

                   nil)
                 

                 ;; DESKTOP HEADER
                 ;;
                 (case cp

                   ;; INDEX
                   ;;
                   :index
                   [des/sticky-header
                    [logo {:on-click logo-on-click}]
                    [toolbar {:state @toolbar-state
                              :class "toolbar toolbar--grid"
                              :show #{:toolbar/all}
                              :on-change handle-toolbar-change}]
                    (index-navigation)]


                   ;; SINGLE
                   ;;
                   :descryptor-single
                   [common/header
                    [logo {:on-click logo-on-click}]
                    [toolbar {:state @toolbar-state
                              :class "toolbar toolbar--single"
                              :show #{:toolbar/theme :toolbar/info}
                              :on-change handle-toolbar-change}]
                    (single-navigation)]

                   nil)))


             ;; BODY
             ;;
             (case cp
               ;;
               ;; INDEX
               ;;
               :index
               [descryptors-grid*
                {:card-opts-fn (fn [coin]
                                 {:tabIndex 1
                                  :on-mouse-down #(.preventDefault %)
                                  :on-key-down (fn [e]
                                                 (condp = (.. e -key)
                                                   "Enter" (do (reset! current-page :descryptor-single)
                                                               (reset! current-coin-idx (:coin-idx coin)))
                                                   false))})

                 :name-opts-fn (fn [coin]
                                 ;; action to do when the user clicks on
                                 ;; the descryptor name link
                                 {:on-click #(do (reset! current-page :descryptor-single)
                                                 (reset! current-coin-idx (:coin-idx coin)))})
                 :tag-opts-fn (fn [_]
                                {:on-click #(let [tag (subs (d/text (.. % -target)) 1)]
                                              ;; toggle tag highlight
                                              (swap! highlight-tags
                                                     (fn [tags]
                                                       (if (get tags tag)
                                                         (disj tags tag)
                                                         (conj tags tag)))))})
                 :visible-charts (:toolbar/charts @toolbar-state)

                 :search-box (if @mobile?
                               (when (or (= @mobile-show :toolbar/search)
                                         (:toolbar/info @toolbar-state))
                                 [search-box
                                  {:state highlight-tags
                                   :props {:class "active"}}])

                               [search-box
                                {:state highlight-tags
                                 :search-icon [:div (inline-html defaults/svg-search)]}])}

                (take @coins-in-grid coins)]


               ;; SINGLE PAGE
               ;;
               :descryptor-single
               [:<>
                [:div.clickback {:on-click logo-on-click
                                 :style {:position :fixed
                                         :top 0
                                         :left 0
                                         :height "100%"
                                         :width "100%"}}]
                
                [descryptor-single*
                 {:tag-opts {:on-click on-tag-click-conj}
                  :mobile? @mobile?}
                 (nth coins @current-coin-idx)]])



             ;; FOOTER TOOLBAR
             ;;
             (when (= cp :index)
               [toolbar {:state @toolbar-state
                         :class "toolbar toolbar--footer"
                         :show #{:footer-toolbar/all}
                         :on-change handle-toolbar-change}])

             ;; FOOTER
             ;;
             (when (= cp :index)
               [footer {:version "build 8e1246a (Hmeli-Suneli)"
                        :support-links ["https://github.com/descryptors/descryptors#support-us"]
                        :logo-opts {:on-click #(window.scroll 0 0)}
                        :terms-opts {:on-click #(swap! app-db assoc :show-terms true)}
                        :about-opts {:on-click #(swap! app-db assoc :show-about true)}
                        :code-opts  {:on-click #(swap! app-db assoc :show-code true)}
                        :contact-opts {:on-click #(swap! app-db assoc :show-contact true)}}])

             
             ;; Contact

             (when (:show-contact @app-db)
               [contact-form {:on-close #(swap! app-db assoc :show-contact false)}])])]))})))



(defn app []
  [index coins])


(defn ^:after-load reload []
  (some->> (sel1 :#app) (rd/render [app])))


(defn ^:export init! []
  (reload))

(init!)
