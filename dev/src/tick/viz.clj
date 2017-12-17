(ns tick.viz
  (:require
   [clojure.xml :refer [parse]]
   [clojure.java.io :as io]
   [tick.alpha.api :as t]
   [clojure.data.xml :as x]
   )
  (:import
   [org.apache.batik.swing JSVGCanvas]
   [javax.xml.parsers DocumentBuilder DocumentBuilderFactory]
   ))

(x/alias-uri :svg "http://www.w3.org/2000/svg")

(def canvas (JSVGCanvas.))

(defn show-canvas []
  (let [frame (javax.swing.JFrame.)]
    (.add (.getContentPane frame) canvas)
    (.setSize frame 800 200)
    (.setDefaultCloseOperation frame javax.swing.JFrame/HIDE_ON_CLOSE)
    (.setVisible frame true)))

(defn until-seconds [a b]
  (.until a b (t/units :seconds)))

(defn label [obj label]
  (with-meta obj {:label label}))

(defn view [& ival-sets]
  (let [tmpfile (java.io.File/createTempFile "tick" ".svg" (io/file (System/getProperty "java.io.tmpdir")))
        _ (assert (every? t/ordered-disjoint-intervals? ival-sets))
        left-edge (apply t/min (map first (apply concat ival-sets)))
        right-edge (apply t/max (map second (apply concat ival-sets)))
        width (until-seconds left-edge right-edge)
        scale (float (/ 800 width))
        bar-height 30
        bar-margin 40]

    (with-open [f (java.io.PrintWriter. (io/writer tmpfile))]
      (x/emit
        {:tag ::svg/svg
         :attrs {:viewBox "0 0 800 800"}
         :content
         (map-indexed
           (fn [y ivals]
             (for [[x1 x2 :as ival] ivals]
               {:tag ::svg/g
                :content
                (let [x (* scale (until-seconds left-edge x1))
                      y (+ bar-margin (* y (+ bar-margin bar-height)))
                      width (* scale (- (until-seconds left-edge x2)
                                        (until-seconds left-edge x1)))]
                  [{:tag ::svg/rect
                    :attrs {:x x
                            :y y
                            :width width
                            :height bar-height
                            :fill "#aaf"
                            :stroke "#88f"
                            :stroke-width "1px"}}

                   (when-some [text (-> ival meta :label)]
                     {:tag ::svg/text
                      :attrs {:x (+ x (/ width 2))
                              :y (- y (/ bar-height 8))
                              :text-anchor "middle"
                              :font-size "130%"}

                      :content (str text)})])}))

           ival-sets)}
        f))

    (.loadSVGDocument canvas (str (.toURL tmpfile)))))
