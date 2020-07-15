(ns proto.descryptors.common
  (:require [taoensso.timbre :refer [info]]
            [proto.charts.util :as pcu :refer [chart-placeholder]]
            [clojure.pprint :refer [cl-format]]
            [proto.descryptors :as des :refer [price-chart github-chart]]
            [proto.descryptors.samples :as samples]
            [proto.descryptors.defaults :as defaults]
            [thi.ng.math.core :as mc]
            #?@(:clj [[clj-time.coerce :as ctc]
                      [clj-time.core :as ct]
                      [clj-time.format :as ctf]]
                :cljs [[proto.util :refer [cc inline-html]]
                       [reagent.core :as r]
                       [proto.dropdown :refer [dropdown]]
                       [proto.toolbar :refer [button]]
                       [goog.string :refer [format]]
                       [goog.string.format]
                       [cljs-time.coerce :as ctc]
                       [cljs-time.core :as ct]
                       [cljs-time.format :as ctf]])))



#?(:clj
   (defmacro cc [args]
     `(~@args)))


#?(:clj
   (defmacro inline-html [html & args]
     `(str ~html ~@args)))



(defn spacefy
  "123456.33555 --> 123 456.34
  cl-format options: ~mincol,padchar,commachar:D"
  [num & [prec]]
  (if (number? num)
    (let [num' (mc/abs* num)
          prec (or prec (if (< 1 num') 0.01 0.001))
          right-len (mc/abs* (Math/log10 prec))
          num' (mc/roundto num' prec)
          left (long num')]

      (if (== num' left)
        (cl-format nil "~,,' :D" left)
        (str (cl-format nil "~,,' :D" left)
             (let [num'' (str num')
                   idx (clojure.string/index-of num'' ".")]
               (subs num'' idx (min (count num'')
                                    (inc (+ idx right-len))))))))
    num))




(defn progress-line [percent]
  [:div.line
   [:div.line__progress
    {:style {:transform (str "translateX(" (- percent 100) "%)")}}]])





(defn supply-indicators [{{:keys [supply]} :metrics :as coin}]
  (let [{:keys [circulating-supply
                max-supply total-supply
                current-supply]} supply
        total-supply (or total-supply circulating-supply)
        max-supply (or (when (and max-supply (< 0 max-supply)) max-supply)
                       total-supply)]
    
    [:div.single__column
     (when max-supply
       [:div.single__column-item
        [:label.single__label "Max Supply"]
        [:div [:span (spacefy max-supply) " " (:symbol coin)]]
        (cc [progress-line 100])])


     (when (and total-supply max-supply (not (zero? max-supply)))
       [:div.single__column-item
        [:label.single__label "Total Supply"]
        [:div [:span (spacefy total-supply) " " (:symbol coin)]]
        (cc [progress-line (* 100 (/ total-supply max-supply))])])
     
     
     (when (and circulating-supply max-supply (not (zero? max-supply)))
       [:div.single__column-item
        [:label.single__label "Circulating Supply"]
        [:div [:span (spacefy circulating-supply) " " (:symbol coin)]]
        (cc [progress-line (* 100 (/ circulating-supply max-supply))])])
     
     (when (and current-supply (not (zero? current-supply))
                max-supply (not (zero? max-supply)))
       [:div.single__column-item
        [:label.single__label "Current Supply"]
        [:div [:span (spacefy current-supply) " " (:symbol coin)]]
        (cc [progress-line (* 100 (/ current-supply (or max-supply current-supply)))])])]))




(defn website [coin]
  (when-let [website (get-in coin [:links :website 0])]
    [:a.website {:href website
                 :target :blank
                 :tabIndex -1}
     [:div.overlay__text.overlay__text--card.overlay__text--website]
     [:div.homepage-icon (inline-html defaults/svg-homepage)]]))




(defn rating [val & [child]]
  (when val
    [:span.rating__wrapper
     [:span.rating val]
     child]))




(defn left-right [props l r]
  [:div props
   [:span.details__left l]
   [:span.details__middle]
   [:span.details__right r]])




(defn tags [xs & [opts]]
  [:h5.tags-wrapper
   (for [tag xs]
     ^{:key tag}
     [:span.tag.sbl
      (cond-> opts
        (= tag "eco") (update :class str " highlight--eco"))
      (str "#" tag)])])




(defn edit-icon [& [opts]]
  #?(:cljs [:a (merge opts (inline-html defaults/svg-edit))]
     :clj  [:a opts (inline-html defaults/svg-edit)]))





(defn founder-icon [& [opts]]
  #?(:cljs [:div.founder-icon (merge opts (inline-html defaults/svg-founder))]
     :clj  [:div.founder-icon opts (inline-html defaults/svg-founder)]))




(defn founder [props founder-name & [opts]]
  [:div.founder props
   [:span founder-name]
   (cc [founder-icon])])




(defn metrics
  [{:as coin :keys [indexes]
    {:keys [price market-data]} :metrics}]
  (let [{:keys [marketcap-index]} (first indexes)]
    [:div
     (if price
       [:h3.overlay-element-holder.price
        (if (< price 0.01)
          (str (format "%.3f" (* price 100)) " cents" )
          (str "$" (spacefy price)))
        (cc [rating (or (some-> marketcap-index inc) "")
             [:div.overlay__text.overlay__text--card.overlay__text--cap-rank]])]
     
       [:h3.placeholder])

     (if-let [change (:price-change-percentage-24h-in-usd market-data)]
       [:h3 {:class (if (pos? change) "price-up" "price-down")
             :title "24h"}
        (str (spacefy change 0.01) "%")]
       [:h3.placeholder])]))





(defn expandable
  "General component for expandable list of data. For Clojure will
  display all data and no expand button, for ClojureScript will
  display amount of data and the expand button."
  [data component
   {:as opts :keys [amount scrollbar? heading btn inner-btn?]
    :or {amount 3}}]

  #?(:clj
     [:div {:class (:class opts)}
      heading
      (component (take amount data))]

     :cljs
     (r/with-let [all-visible? (r/atom false)
                  scroll-wrap  (if scrollbar?
                                 [des/overlay-scrollbars {:dynamic? true}]
                                 [identity])
                  btn-component #(when (and btn (pos? %))
                                   [btn all-visible? %])]
       
       (let [remaining (- (count data) amount)]
         [:div {:class (:class opts)}
          heading
          (conj
           scroll-wrap
           [component
            (cond->> data
              (not @all-visible?) (take amount))
            (when inner-btn?
              [btn-component remaining])])

          (when-not inner-btn?
            [btn-component remaining])]))))






;; Exchanges
;;

(def exchange-timestamp (pcu/date-formatter "hh:mma"))


#?(:cljs
   (defn show-all-less-btn [all-visible?]
     [button {:label (if @all-visible? "Show less" "Show all")
              :class "more--rounded"
              :on-click #(swap! all-visible? not)}]))




(defn exchange-row [props {:keys [idx rank name url timestamp
                                  spread-avg volume volume-usd]
                           {:keys [usd eth btc]} :targets}]
  [:div.table__row props
   [:div.table__col.table__col--number idx]
   [:div.table__col.table__col--name (if url
                                       [:a {:href url :target :blank} name]
                                       name)]
   (when usd [:div.table__col.table__col--pair (spacefy usd)])
   ;;(when btc [:div.table__col.table__col--pair (spacefy btc)])
   ;;(when eth [:div.table__col.table__col--pair (spacefy eth)])
   [:div.table__col.table__col--vol24 (spacefy volume 0.01)]
   [:div.table__col.table__col--vol24 (spacefy volume-usd 0.01)]
   [:div.table__col.table__col--vol24 (spacefy spread-avg)]
   #_[:div.table__col.table__col--price price]
   #_[:div.table__col.table__col--vol-percent vol-percent]
   #_[:div.table__col.table__col--category category]
   #_[:div.table__col.table__col--fee fee]
   [:div.table__col.table__col--updated timestamp]])




(defn exchanges-render [sym exchanges]
  [:div.table

   ;; Header
   ;;
   (cc [exchange-row {:key "header"}
        {:idx "#" :name "Exchange"
         :targets (some->> (:targets (first exchanges))
                           keys
                           (reduce
                            (fn [m k]
                              (->> (name k)
                                   clojure.string/upper-case
                                   (str sym "/")
                                   (assoc m k)))
                            {}))
         :volume "Volume" :volume-usd "Volume (USD)"
         :spread-avg "Spread (Avg)"
         :timestamp "Updated On (GMT)"}])

   ;; Exchange Data
   ;;
   (->> exchanges
        (map-indexed
         (fn [idx exchange]
           (cc [exchange-row {:key (:id exchange)}
                (-> exchange
                    (update :timestamp exchange-timestamp)
                    (assoc :idx (inc idx)))]))))])




(defn exchanges [sym exchanges]
  (cc [expandable exchanges
       (partial exchanges-render sym)
       {:amount 4
        :scrollbar? true
        :class "single__row single__row--table"
        #?@(:cljs [:btn show-all-less-btn])}]))






;; Social Media
;;


(defn social-media-render [posts]
  [:div.media
   (for [{:keys [score title subtitle num-comments url]} posts]
     ^{:key title}
     [:div.media__item
      [:div.media__col.media__col--number
       [:div.media__number score]]

      [:div.media__col.media__col--title
       [:div.media__title--main
        (into
         [:a.media__link--main {:href url :target :blank}]
         ;; tokenize #hashtags and @users
         (->> (clojure.string/split title #"(@\w*|#\w*)")
              (map (fn [s]
                     (condp = (first s)
                       "#" [:span.media__title--tag s]
                       "@" [:span.media__title--user s]
                       s)))))]
       [:div.media__title--second subtitle]]

      (when (some->> num-comments (< 0))
        [:div.media__col.media__col--messages
         [:div.media__number num-comments]
         [:div.media__icon
          (inline-html defaults/svg-comment)]])])])



(defn social-media [title posts]
  (cc [expandable posts (partial social-media-render)
       {:amount 5
        :heading [:div.media__heading title]
        :class "single__row single__row--social"
        #?@(:cljs [:btn show-all-less-btn])}]))





;; Wallets
;;


#?(:cljs
   (defn show-more-dots-btn [all-visible?]
     (when-not @all-visible?
       [:div.more.more--dots.more--transparent
        {:on-click #(swap! all-visible? not)}
        [:span "..."]])))



(defn wallets-render [ws & [more-btn]]
  [:div.wallets
   (map (fn [[slug url]]
          ^{:key slug} [:a.wallet {:href url :target :blank} slug])
        ws)
   more-btn])



(defn wallets [ws]
  (cc [expandable ws (partial wallets-render)
       {:amount 5
        :heading [:label.single__label "Wallets: "]
        :class "single__row single__row--wallets"
        :inner-btn? true
        #?@(:cljs [:btn show-more-dots-btn])}]))





;; Team
;;


#?(:cljs
   (defn show-more-number-btn [all-visible? remaining]
     (when-not @all-visible?
       [:div.more.more--number
        {:on-click #(swap! all-visible? not)}
        (str "+" remaining)])))



(defn team-render [xs & [more-btn]]
  (if (not-empty xs)
    [:div
     (for [[x roles] xs]
       (let [wrapper (cond
                       (:founder roles) founder
                       :else :div)]
         (cond
           (fn? wrapper) (cc [wrapper {:key x} x])
           :else [wrapper {:key x} x])))
     more-btn]
    [:div.placeholder]))



(defn team [xs]
  (cc [expandable xs (partial team-render)
       {:amount 5
        :heading [:label.single__label "People"]
        :class "single__column-item"
        :inner-btn? true
        #?@(:cljs [:btn show-more-number-btn])}]))




;; Used By
;;

(defn used-by-render [xs & [more-btn]]
  (if (not-empty xs)
    [:div
     (for [x xs]
       ^{:key x} [:div x])
     more-btn]
    [:div.placeholder]))



(defn used-by [xs]
  (cc [expandable xs (partial used-by-render)
       {:amount 3
        :heading [:label.single__label "Used by"]
        :class "single__column-item"
        :inner-btn? true
        #?@(:cljs [:btn show-more-number-btn])}]))



(defn kebab->capital [s]
  (-> (name s)
      (.split "-")
      (->> (map clojure.string/capitalize)
           (interpose " ")
           (apply str))))



(defn coin-name [coin]
  (or (:name coin)
      (kebab->capital (:slug coin))))


;; Single Page
;;


(defn descryptor-single
  [{:as opts :keys [tag-opts more-on-click mobile?]}
   {:as coin :keys [similar four-words description
                    indexes links]
    {github-data :github price-data :price
     reddit-data :reddit twitter-data :twitter
     exchanges-data :exchanges} :data
    metrics-data :metrics
    wallets-data :wallets}]

  (let [{:keys [github-index]} (first indexes)]
    
    [:div.single {:key (:slug coin)}
     
     ;; Similar coins
     ;;
     [:div.single__header
      (when similar
        [:div.similar-coins
         [:label.single__label "Similiar:"]
         (for [slug similar]
           ^{:key slug} [:a.coin {:href (str "/" slug)} slug])])

      #?(:cljs
         (when similar
           [:div.more.more--dots.more--similar.more--bg
            {:on-click more-on-click}
            [:span "..."]]))


      (cc [edit-icon
           {:href "https://reddit.com/r/descryptors"
            :class :edit
            :target :blank}])]


     ;; coin information
     ;;
     [:div.single__row.single__row--info
      [:div.single__column
       [:h1.overlay-element-holder.name-wrapper
        [:span (coin-name coin)]
        (cc [website coin])]

       (when-not (empty? (:tags coin))
         (cc [tags (:tags coin) tag-opts]))

       [:h2 (:symbol coin)]

       (cc [metrics coin])

       (let [{:keys [whitepaper reddit twitter code]} links]
         [:div.single__link-wrapper
          (when-let [link (first whitepaper)]
            [:a.single__link {:href link
                              :target :blank}
             "whitepaper"])

          (when-let [link (first code)]
            [:a.single__link {:href link
                              :target :blank}
             "code"])
          
          (when-let [link (first reddit)]
            [:a.single__link {:href link
                              :target :blank}
             "reddit"])
          
          (when-let [link (first twitter)]
            [:a.single__link {:href link
                              :target :blank}
             "twitter"])
          
          #_[:div.single__link "reddit"]
          #_[:div.single__link "documentation"]
          #_[:div.single__link "roadmap"]
          #_[:div.single__link "wikipedia"]])]


      ;; People
      
      [:div.single__column
       (cc [team (:people coin)])
       (cc [used-by (:used-by coin)])]

      
      [:div.single__column
       
       #_[:div.single__column-item
          [:label.single__label "Transaction Speed"]
          [:div [:span "x10 faster then BTC"]]]

       #_[:div.single__column-item
          [:label.single__label "Transaction Fee"]
          [:div [:span "0.001"][:span "$"]]]

       (when-let [marketcap (get-in metrics-data [:market-data :market-cap])]
         [:div.single__column-item
          [:label.single__label "Market Cap"]
          [:div [:span "$"]
           [:span (spacefy (long marketcap))]]])

       #_[:div.single__column-item
          #?@(:clj [[:label.single__label.single__label--dropdown "Volume 24h"]]
              :cljs [[:label.single__label.single__label--dropdown "Volume 24h"]]
              #_[[:label.single__label.single__label--dropdown "Volume"]
                 [dropdown
                  {:text "24h"
                   :value "24h"
                   :container-class "dropdown dropdown--single"
                   :on-change identity ;;#(reset! current-theme (:value %))
                   :content (for [[id text] [[:24h "24h"] [:1w "1w"] [:6m "6m"] [:1y "1y"]]]
                              {:on-click identity ;;#(on-click-theme id)
                               :key text
                               :text text
                               :value id})}]])

          [:div [:span "$"]
           [:span (spacefy
                   (long (:volume-24h
                          (:market-data metrics-data))))]]]]

      ;; bar indicators
      (cc [supply-indicators coin])]


     ;; Wallets

     (when (not-empty wallets-data)
       (cc [wallets wallets-data]))
     
     
     ;; Price Chart
     ;;
     [:div.single__row.single__row--chart

      ;; Chart

      [:div.single__column.single__column--lg
       [:div.single__column-item

        (let [{:keys [svg data]} price-data
              {{svg-data :data svg-spec :spec} defaults/chart-period} svg]

          (when-not (and (empty? data)
                         (empty? svg-data)
                         (nil? svg-spec))
            (cc [price-chart price-data
                 {:period-selectors? true
                  :xticks? true
                  :yticks? true
                  :dynamic? #?(:clj false :cljs true)
                  :view defaults/price-period-single
                  :spec {:line-width "1.2px"
                         :grid-width #?(:clj "1px"
                                        :cljs (if js/isChrome "0.9px" "1px"))
                         :size (get-in defaults/dynamic-chart-sizes
                                       (if mobile?
                                         [:price :mobile]
                                         [:price :desktop]))}}])))]]

      ;; Info

      (when (:market-data metrics-data)
        [:div.single__column.single__column--sm
         [:div.single__column-item.details

          [:div.details__column
           [:span {:class "single__label"}
            "Price Change"]

           ;; don't show last updated for now
           #_(when-let [last-updated
                        (some->> (get-in metrics-data [:market-data :last-updated])
                                 (ctc/from-long)
                                 (ctf/unparse #?(:cljs {:format-str "hh:mma"}
                                                 :clj (ctf/formatter "hh:mma"))))]

               [:span {:class "details__updated"}
                "5 minutes ago"]
             
               #_(cc [left-right {:class "details__row details__row--small"}
                      [:span "Last updated"]
                      (str last-updated " GMT")]))
         
           (let [changes [[:price-change-percentage-1h-in-usd "1h"]
                          [:price-change-percentage-24h-in-usd "24h"]
                          [:price-change-percentage-7d-in-usd "7d"]]]
           
             (for [[data-key text] changes]
               (when-let [change (get-in metrics-data [:market-data data-key])]
                 (cc [left-right {:class "details__row details__row--small"
                                  :key data-key}
                      text [:span {:class (if (pos? change) "price-up" "price-down")}
                            (str (spacefy change) "%")]]))))]


          ;; Blockchain Stats
        
          (let [stats [[:count-of-tx "Transactions"]
                       [:count-of-blocks-added "Blocks added"]
                       [:count-of-active-addresses "Active addresses"]
                       [:median-tx-value "Avg. transaction (USD)"]
                       [:median-tx-fee "Avg. fee (USD)"]]]
          
            (when (->> (vals (:blockchain-stats-24-hours metrics-data))
                       (filter (partial < 0))
                       (not-empty))
              [:div.details__column
               [:span {:class "single__label"}
                "Blockchain Stats"]

               (for [[data-key text] stats]
                 (when-let [stat (get-in metrics-data [:blockchain-stats-24-hours data-key])]
                   (when (< 0.001 stat)
                     (cc [left-right {:class "details__row details__row--small"
                                      :key data-key}
                          text [:span (spacefy stat)]]))))]))]])]



     ;; Github Chart
     ;;
     [:div.single__row.single__row--chart

      ;; Chart

      [:div.single__column.single__column--lg
       [:div.single__column-item
        (let [{:keys [svg data]} github-data
              {{svg-data :data svg-spec :spec} defaults/chart-period} svg]
          (when-not (and (empty? data)
                         (empty? svg-data)
                         (nil? svg-spec))

            (cc [github-chart github-data
                 {:period-selectors? true
                  :dynamic? #?(:clj false :cljs true)
                  :view defaults/git-period-single
                  :spec {:size (get-in defaults/dynamic-chart-sizes
                                       (if mobile?
                                         [:git :mobile]
                                         [:git :desktop]))}}])))]]


      ;; Info

      [:div.single__column.single__column--sm
       [:div.single__column-item.details
        (when-let [total (:total github-data)]
          (let [repos (:repos github-data)]
            [:div.details__column
             (cc [left-right {:class "details__row details__row--big details--github"}
                  [:span "Code Activity"
                   (cc [rating (or (some-> github-index inc) "")
                        [:div.overlay__text.overlay__text--card.overlay__text--dev-rank]])]
                  total])

             (for [{:keys [repo total]} repos]
               (cc [left-right {:class "details__row details__row--small"
                                :key repo} repo total]))]))]]]

     

     (when (not-empty exchanges-data)
       (cc [exchanges (:symbol coin) exchanges-data]))


     (when-let [posts (not-empty (:posts reddit-data))]
       (cc [social-media "Latest Reddit Posts"
            (reverse (sort-by :created posts))]))
     

     (when-let [tweets (not-empty (:tweets twitter-data))]
       (cc [social-media "Latest Tweets"
            (reverse (sort-by :created tweets))]))]))





;; Grid
;;


(defn descryptor
  [{:as coin :keys [indexes]
    {github-data :github price-data :price} :data}
   & [{:keys [card-opts tag-opts name-opts visible-charts]}]]

  (let [{:keys [github-index]} (first indexes)]
    ^{:key (:slug coin)}
    [:div.descryptor card-opts
     [:div.upper-part
      [:h1.overlay-element-holder.name-wrapper
       [:a name-opts
        [:span.sbl (coin-name coin)]]
       (cc [website coin])]

      (cc [tags (:tags coin) tag-opts])]
   
     [:div.bottom-part
      (if (:symbol coin)
        [:h2 [:span.sbl (:symbol coin)]]
        [:h2.placeholder])

      (cc [metrics coin])

      ;; Price Chart
      ;;
      (when (:price-chart visible-charts)
        (let [{{{svg-spec :spec svg-data :data} defaults/chart-period} :svg} price-data]
          (if svg-data
            (cc [price-chart price-data])
            (cc [chart-placeholder {:size defaults/price-chart-size}]))))


      ;; Github Info
      ;;
      (when (:github-chart visible-charts)
        [:div.details.details--github
         (let [total (:total github-data)]
           (if (and total (not (zero? total)))
             (cc [left-right {:class "details__row details__row--middle"}
                  [:span "Code activity"
                   (cc [rating (or (some-> github-index inc) "")
                        [:div.overlay__text.overlay__text--card.overlay__text--dev-rank]])]
                  total])
           
             [:div.details__row.details__row--middle.placeholder.full]))])

      
      ;; Github Chart
      ;;
      (when (:github-chart visible-charts)
        (let [{{{svg-spec :spec svg-data :data} defaults/chart-period} :svg} github-data]
          (if svg-data
            (cc [github-chart github-data
                 {:spec {:size defaults/github-chart-size}}])
            (cc [chart-placeholder
                 {:size defaults/github-chart-size}]))))]]))




(defn descryptors-grid
  [{:as opts :keys [search-box card-opts-fn name-opts-fn tag-opts-fn]} coins]
  
  (into
   [:div.descryptors-grid

    search-box]

   (mapv
    (fn [coin]
      (cc [descryptor coin
           (cond-> opts
             card-opts-fn (update :card-opts merge (card-opts-fn coin))
             name-opts-fn (update :name-opts merge (name-opts-fn coin))
             tag-opts-fn  (update :tag-opts  merge (tag-opts-fn coin)))]))
    coins)))




(defn navigation
  [& [{:keys [prev-opts next-opts]}]]
  [:div.navigation
   [:a.navigation__item.prev
    #?@(:cljs [(merge prev-opts (inline-html defaults/svg-prev))]
        :clj  [prev-opts (inline-html defaults/svg-prev)])]
   
   [:div.navigation__item.break.break--diagonal]
   
   [:a.navigation__item.next
    #?@(:cljs [(merge next-opts (inline-html defaults/svg-next))]
        :clj  [next-opts (inline-html defaults/svg-next)])]])




(defn logo
  [& [opts]]
  [:a.logo opts
   [:div.logo-wrapper
    (inline-html defaults/svg-logo)]])




(defn header [& children]
  (into
   [:div.header]
   children))




(def support-links
  [(cc [des/clipboard
        [:a.footer__support-tag
         {:data-clipboard-text ",,,"
          #?@(:clj
              [:href ",,,"
               :target :blank])}
         "support link"]])])



;; todo: don't align vertical for < 550px, but keep them horizontal

(defn footer-support [support-links]
  #?(:cljs
     (r/with-let [show-icons? (r/atom false)]
       (into
        [:div.footer__support-wrapper]
        (if @show-icons?
          support-links
          [[:a.footer__support-link
            {:on-click #(reset! show-icons? true)}
            "Support Us"]])))

     :clj
     (into
      [:div.footer__support-wrapper
       [:span.footer__support-link "Support Us"]]
      support-links)))




(defn footer [& [{:as opts :keys [logo-opts contact-opts
                                  code-opts reddit-opts
                                  terms-opts about-opts version
                                  support-links]}]]
  [:div.footer
   [:div.footer__top-row
    (cc [logo logo-opts])

    #?(:cljs
       [:div.footer__link-wrapper
        [:a.footer__link about-opts
         "About"]
        [:a.footer__link terms-opts
         "Terms"]
        [:a.footer__link code-opts
         "Code"]
        [:a.footer__link reddit-opts
         "Reddit"]
        [:a.footer__link contact-opts
         "Contact"]])

    (cc [footer-support support-links])]

   [:div.footer__bottom-row
    [:div.footer__copyright
     [:span "©"]
     [:span "2019 Descryptors."]
     [:span "All rights reserved"]]

    [:div.footer__copyright.footer__copyright--version
     [:span version]]]])

