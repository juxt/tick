(ns tick.reflection-test
  "tick.core must compile with no reflection warnings, so that a GraalVM
  native image of a program that uses tick needs no reflection config."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(defn- reflection-warnings
  "Recompiles `ns-sym` with *warn-on-reflection* on and returns the
  reflection warnings that the compiler printed."
  [ns-sym]
  (let [err (java.io.StringWriter.)]
    (binding [*warn-on-reflection* true
              *err*                err]
      (require ns-sym :reload))
    (->> (str/split-lines (str err))
         (filter #(str/includes? % "Reflection warning")))))

(deftest tick-core-has-no-reflection
  (is (= [] (reflection-warnings 'tick.core))))
