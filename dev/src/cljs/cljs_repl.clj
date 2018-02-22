 (ns cljs-repl)

(require
  '[cljs.repl :as cljs-repl]
  '[cljs.repl.node :as node])

(cljs-repl/repl* (node/repl-env)
  {:output-dir "out"
   :optimizations :none
   :cache-analysis true
   :source-map true})

 