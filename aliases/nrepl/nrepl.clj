(ns ^{:clojure.tools.namespace.repl/load false} nrepl
  (:require
   [clojure.tools.nrepl.server :as nrepl.server]
   [cider.nrepl]
   ;;[refactor-nrepl.middleware :as refactor.nrepl]
   [io.aviso.ansi]))

(defn start-nrepl
  [opts]
  (let [server
        (nrepl.server/start-server
          :port (:port opts)
          :handler
          (apply nrepl.server/default-handler
                 (conj (map #'cider.nrepl/resolve-or-fail cider.nrepl/cider-middleware)
                       ;; #'refactor.nrepl/wrap-refactor
                       )))]
    (spit ".nrepl-port" (:port server))
    (println (io.aviso.ansi/yellow (str "[tick] nREPL client can be connected to port " (:port server))))
    server))

(println "[tick] Starting nREPL server")

(def server (start-nrepl {:port 5610}))
