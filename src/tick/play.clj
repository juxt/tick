(ns tick.play
  (:require
   [clojure.xml :refer [parse]]
   [clojure.java.io :as io])
  (:import
   [org.apache.batik.swing JSVGCanvas]
   [javax.xml.parsers DocumentBuilder DocumentBuilderFactory]
   ))




(def doc
  (.parse
   (.newDocumentBuilder (DocumentBuilderFactory/newInstance))
   (io/file "clock2.svg")))



(def canvas (JSVGCanvas.))

(.loadSVGDocument canvas "file:calendar-151591.svg")

;;(.setDocument canvas doc)

(defn show-canvas [canvas]
  (let [frame (javax.swing.JFrame.)]
    (.add (.getContentPane frame) canvas)
    (.setSize frame 800 200)
    (.setDefaultCloseOperation frame javax.swing.JFrame/HIDE_ON_CLOSE)
    (.setVisible frame true)))


(show-canvas canvas)
