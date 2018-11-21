(ns tick.docs.app
  (:require-macros [tick.docs.app :refer [analyzer-state]])
  (:require
    [reagent.core :as r]
    [tick.alpha.api :as t]
    [tick.timezone]
    [clojure.string :refer [lower-case capitalize]]
    [cljs.js :refer [empty-state eval js-eval eval-str]]
    [cljs.tools.reader :refer [read-string]]
    [cljs.env :as env]))

(def state (cljs.js/empty-state))

(defn eval-code [source cb label]
  (cljs.js/eval-str 
    state
    ; don't know how to do namespacing
    (clojure.string/replace source "t/" "tick.alpha.api/")
    (str "[" label "]") 
    {:eval cljs.js/js-eval :context :expr} 
    cb))

(defn load-library-analysis-cache! []
  (cljs.js/load-analysis-cache! state 'tick.alpha.api (analyzer-state 'tick.alpha.api))
  ;(cljs.js/load-analysis-cache! state 't (analyzer-state 'tick.alpha.api))
  (cljs.js/load-analysis-cache! state 'tick.timezone (analyzer-state 'tick.timezone))
  nil)

(defn day-midnight-today []
  (t/day-of-week (t/end (t/bounds (t/today)))))

(defn day-midnight-tomorrow []
  (t/day-of-week (t/end (t/bounds (t/tomorrow)))))

(defn two-days-from-today []
  (str "on " (capitalize (str (day-midnight-tomorrow))) " morning"))

(defn button [label cb]
  [:button
   {:key label
    :onClick cb}
   label])

(defn code-component [code result label]
  [:div
   [:div.content
    [:pre.highlight
     [:code.language-clojure {:data-lang "clojure"} code (when-let [v @result] (str " => " v))]
     [:div.code-buttons
      (list
        (button "Eval" (fn [ev]
                         (eval-code code
                                    (fn [x]
                                      (println (pr-str x))
                                      (reset! result (str (:value x))))
                                    label)))
        (button "Clr" (fn [ev]
                        (reset! result nil)
                        )))]]]])

(defn interval-relations [config *value]
  (let [value (js/parseInt @*value 10) ; sometimes we get passed strings!
        width 720
        x-cells 10
        cell-width (/ width x-cells)
        fixed-block-width-in-cells 4
        fixed-block-width (* fixed-block-width-in-cells cell-width)
        block-width-in-cells 2
        block-width (* block-width-in-cells cell-width)
        min 0
        max (- x-cells block-width-in-cells)

        now (t/now)
        ->time #(t/+ now (t/new-duration (inc %) :seconds))

        ival1 (t/new-interval
                (->time value)
                (->time (+ value block-width-in-cells)))

        ival2 (t/new-interval
                (->time (- (/ x-cells 2) (/ fixed-block-width-in-cells 2)))
                (->time (+ (/ x-cells 2) (/ fixed-block-width-in-cells 2))))]

    [:div.diagram
     [:div
      [:p
       [:input {:style {:width "100%"}
                :type :range
                :value value
                :min min :max max
                :onChange (fn [ev]
                            (reset! *value (.-value (.-target ev))))}]]]

     [:svg {:viewBox [0 0 width 40]}
      [:rect
       {:x (* cell-width value) :y 10 :width block-width :height 8 :fill "orange"}]

      ;; fixed
      [:rect
       {:x (- (/ width 2) (/ fixed-block-width 2)) :y 30 :width fixed-block-width :height 8 :fill "#444"}]]

     (letfn [(f [rel] (case rel
                        :precedes "precedes"
                        :meets "meets"
                        :starts "starts"
                        :during "is during"
                        :finishes "finishes"
                        :overlaps "overlaps"
                        :contains "contains"
                        :overlapped-by "is overlapped by"
                        :started-by "is started by"
                        :finished-by "is finished by"
                        :met-by "is met by"
                        :preceded-by "is preceded by"
                        ))]
       [:div
        [:p "The higher interval "
         [:em (f (t/relation ival1 ival2))]
         " the lower interval, whereas the lower interval "
         [:em (f (t/relation ival2 ival1))]
         " the higher interval."]
        [:p "Relation between higher and lower interval: " [:tt (pr-str (t/relation ival1 ival2))]]
        [:p "Relation between lower and higher interval: " [:tt (pr-str (t/relation ival2 ival1))]]])]))

(defonce code-blocks
  (for [el (array-seq (.querySelectorAll js/document ".code"))]
    {:el el
     :id (.-id el)
     :code (.-innerText (.querySelector el "pre"))
     :result (r/atom nil)}))

(defonce interval-relation-diagrams
  (for [el (array-seq (.querySelectorAll js/document ".interval-relations"))]
    {:el el
     :id (.-id el)
     :config (.-innerText (.querySelector el "pre"))
     :value (r/atom 0)}))

(defn init []

  ;; Read this and weep: https://github.com/arichiardi/replumb/commit/339fe2aa39bb794ca34710317b109bf07916de27
  (js* "goog.isProvided_ = function(x) { return false; };")

  (.log js/console "Starting up…")

  (load-library-analysis-cache!)

  (r/render [two-days-from-today]
            (.getElementById js/document "eval-two-days-from-today"))

  (doseq [{:keys [id el code result]} code-blocks]
    (r/render [code-component code result id] el))

  (doseq [{:keys [el config value]} interval-relation-diagrams]
    (r/render [interval-relations config value] el)))

(init)
