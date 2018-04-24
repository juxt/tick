(ns tick.rebel.main
  (:require
    rebel-readline.clojure.main
    rebel-readline.core
    io.aviso.ansi))

(defn -main
  [& args]
  (rebel-readline.core/ensure-terminal
    (rebel-readline.clojure.main/repl
      :init (fn []
              (try
                (println "[tick] Loading Clojure code, please wait...")
                (require 'user)
                (in-ns 'user)
                (catch Exception e
                  (.printStackTrace e)
                  (println "[tick] Failed to require user, this usually means there was a syntax error. See exception above.")))))))
