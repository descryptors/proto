(ns proto.util
  (:require [goog.functions]
            [clojure.pprint :refer [cl-format]]
            [goog.string :refer [format]]
            [clojure.string :refer [split]])
  (:require-macros [proto.util]))


(defn coll->pattern [coll]
  (re-pattern
   (str "(?i)\\b("
        (->> (interpose "|" coll)
             (apply str))
        ")\\b")))


(defn expand-descendants [coll]
  (reduce #(if-let [des (descendants %2)]
             (into %1 des)
             (conj %1 %2))
          #{}
          coll))



(defn find-item [item coll]
  (->> coll
       (keep-indexed #(when (= %2 item) %1))
       (first)))


(defn limiter [rate]
  (goog.functions/rateLimit #(when (fn? %) (%)) rate))


(defn debouncer [rate]
  (goog.functions/debounce #(when (fn? %) (%)) rate))


(defn throttler [rate]
  (goog.functions/throttle #(when (fn? %) (%)) rate))



(defn toggle-set-item [coll item]
  (let [coll (set coll)]
    ((if (get coll item) disj conj) coll item)))



;; Track Window Size

(defn match-media? [breakpoint]
  (as-> breakpoint $
    (str "(max-width: " $ "px)")
    (.matchMedia js/window $)
    (aget $ "matches")))


(defn mobile-tracker [ratom mobile-breakpoint]
  (fn []
    (let [set-size!
          (fn []
            (let [mobile?      (match-media? mobile-breakpoint)
                  inner-width  (some-> js/window .-innerWidth)
                  client-width (some-> js/document .-body .-clientWidth)
                  page-width   (or inner-width client-width)]
              (reset! ratom mobile?)))]
      (set-size!))))
