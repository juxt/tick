(ns infra
  (:require
   [clojure.tools.nrepl.server :as nrepl.server]
   [cider.nrepl]))

(defn start-nrepl
  []
  (let [server
        (nrepl.server/start-server
          :handler
          (apply nrepl.server/default-handler
                 (-> (map #'cider.nrepl/resolve-or-fail cider.nrepl/cider-middleware)
                     #_(conj #'refactor.nrepl/wrap-refactor))))]
    (spit ".nrepl-port" (:port server))
    server))

(defonce nrepl (start-nrepl))
