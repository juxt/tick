(ns build
  (:require [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]
            [clojure.java.shell :as sh]
            [clojure.string :as string]))
(def lib 'tick/tick)
(def version (some-> (sh/sh "git" "describe" "--tags" "--abbrev=0")
                     :out
                     (string/trim-newline)))

(println "version " version)
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(defn clean [_]
  (b/delete {:path "target"})
  (sh/sh "rm" "-rf" "web-target/public/*")
  )

(defn jar [_]
  (b/write-pom {:src-pom "dev/resources/src-pom.xml"
                :class-dir class-dir
                :lib lib
                :version version
                :basis basis
                :src-dirs ["src"]})
  (b/copy-dir {:src-dirs ["src"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))

(defn install [_]
  (b/install {:basis      basis
              :lib        lib
              :version    version
              :jar-file   jar-file
              :class-dir  class-dir})
  (println (str "clj -Sdeps '{:deps {tick/tick {:mvn/version \"" version "\"}}}'"))
  )

(defn deploy [_]
  (dd/deploy {:installer :remote
              :artifact jar-file
              :pom-file (b/pom-path {:lib lib :class-dir class-dir})}))