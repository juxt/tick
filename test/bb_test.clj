
deps -A:test -Scommand "bb -cp {{classpath}}"


(require '[tick.alpha.api-test])
(in-ns 'tick.alpha.api-test)
(clojure.test/run-all-tests)