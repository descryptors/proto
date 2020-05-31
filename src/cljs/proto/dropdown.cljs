(ns proto.dropdown
  (:require [goog.dom :as dom]
            [proto.mixins :as mixins]
            [reagent.core :as r]
            [reagent.dom :as rd]))


;; adapted from metosin/komponentit


(defn ->menu-item
  "- `:key` React key
- `:value` (Optional) Current value
- `:on-click` (Optional) callback
- `:el` (Default: `:option`)
- `:props` (Optional) React props to dropdown list item element
- `:text` Text"
  [open?
   {:keys [on-change close-on-click?
           active-item-class
           menu-item-class]
    :or   {active-item-class ""}
    :as   dropdown-opts}
   {:keys [el props key text value on-click]
    :or   {el :option}
    :as   item-opts}]
  (let [menu-item-class (or (:menu-item-class item-opts)
                            (:menu-item-class dropdown-opts)
                            "")]

    ^{:key key}
    [el
     (merge props
            {:class    (str menu-item-class " " (:class props) " "
                            (if (and value (= (:value dropdown-opts) value))
                              active-item-class))
             :on-click (fn [e]
                         (.preventDefault e)
                         (if-not (false? close-on-click?) (reset! open? false))
                         (if on-click (on-click e))
                         (if on-change (on-change item-opts))
                         nil)})
     text]))


(defn toggle [open? e]
  (.preventDefault e)
  (swap! open? not)
  nil)


(defn dropdown
  "- `:text` Text for dropdown toggle element.
- `:content` Used to pass in dropdown elements as collection of item-maps.
- `:on-change` (Optional, supported by content) Called with item-map when dropdown item is selected.
- `:value` (Optional, supported by content) Current value, marks list item with equivalent value as active.
- `:close-on-click?` (Default `true`) Close dropdown when any item is selected.
- `:caret?` (Default `true`) Whether to automatically append caret to text.

HTML elements:

- `:container-el` (Default `:div`) The element used to contain toggle element and dropdown list.
- `:el` (Default `:span`) The toggle element type.

CSS classes:

- `:container-class` (Default `\"dropdown-container\"`)
- `:open-class` (Default `\"open\"`)
- `:dropdown-class` (Default `\"dropdown-menu\"`)
- `:menu-item-class` (Default `\"dropdown-menu__item\"`)
- `:active-item-class` (Default `\"dropdown-menu__item--active\"`)

Props:

Any other React props can be passed to the created elements:

- `:toggle-props`
- `:container-props`
- (Item option) `:props`
- (Item option) `:link-props`"
  
  [{:keys [text on-change]}]
  
  (let [open? (r/atom false)
        text* (r/atom text)
        on-change* #(do (reset! text* (:text %))
                        (when on-change (on-change %)))]
    (fn
      [{:as   props
        :keys [content
               el
               container-el
               open-class
               toggle-class
               class
               dropdown-class
               toggle-props
               container-props
               container-class]
        :or   {el              :span
               container-el    :div
               toggle-class    "dropdown__placeholder"
               open-class      "dropdown--istoggle"
               dropdown-class  "dropdown__option-wrapper"
               container-class "dropdown"}}]

      (let [props (assoc props :on-change on-change*)]
        
        [container-el
         (merge container-props
                {:class    (str container-class " " (when @open? open-class))
                 :on-click (partial toggle open?)})
         [el
          (merge toggle-props
                 {:class (str toggle-class " " (:class toggle-props))})
          @text*]
         
         (if @open?
           [mixins/window-event-listener
            {:on-click    (fn [e]
                            ;; If the click target is outside of navbar
                            (if (not (dom/contains (rd/dom-node (r/current-component)) (.. e -target)))
                              (reset! open? false)))
             :on-key-down (fn [e]
                            (case (.-keyCode e)
                              ;; Esc
                              27 (reset! open? false)
                              nil))}
            [:div
             {:class (str dropdown-class)}
             (map (partial ->menu-item open? props) content)]])]))))


;; TODO: Key handlers, Up/down, enter.
