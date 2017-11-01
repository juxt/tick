(ns tick.matrix
  (:require [clojure.core.matrix :as m]))

(m/inner-product [[1 1 1]
                  [1 1 1]
                  [1 1 1]]

                 [[1 1 1]
                  [1 1 1]
                  [1 1 1]])

[[0.45752 0       240.57]
 [0       0.45752 302.32]
 [0       0       1]]
