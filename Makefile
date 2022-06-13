# Authoritative build rules for tick.

# If you don't have GNU Make on your system, use this file as a
# cribsheet for how to build various aspects of tick.

STYLESDIR = .
STYLESHEET = juxt.css

.PHONY: 		watch default deploy test dev-docs-cljs docs/index.html

default:		docs/index.html

# Build the docs
docs/index.html:	docs/*.adoc docs/docinfo*.html ${STYLESDIR}/${STYLESHEET}
			asciidoctor -d book \
			-a "webfonts!" \
			-a stylesdir=../${STYLESDIR} \
			-a stylesheet=${STYLESHEET} \
			-o $@ \
			docs/index.adoc

# this just works on henryw374s machine. at some point docs will get hosted somewhere else
deploy-docs:
			make && cd docs && firebase deploy
test-clj:
			./bin/kaocha :clj
test-node:
			rm -rf cljs-test-runner-out && mkdir -p cljs-test-runner-out/gen && clojure -Sverbose -M:test-node
# For developing the cljs used by the documentation, starts a REPL and a server at localhost:9500
dev-docs-cljs:
			clojure -M:docs-watch
# Builds production js and html files
release-docs-cljs: 
			make && clojure -A:docs-release
test-cljs-shadow:
			clojure -Atest-cljs -X com.widdindustries.tiado-cljs2/tests-ci-shadow :compile-mode :release
test:
			make test-clj && make test-cljs
clean:
			clj -T:build clean
install:
			make clean && clj -T:build jar && clj -T:build install \
			&& mkdir -p tmp && cd tmp
deploy:
			clj -T:build deploy
lint:
			clj-kondo --lint src test

# hooray for stackoverflow
.PHONY: list
list:
		@$(MAKE) -pRrq -f $(lastword $(MAKEFILE_LIST)) : 2>/dev/null | awk -v RS= -F: '/^# File/,/^# Finished Make data base/ {if ($$1 !~ "^[#.]") {print $$1}}' | sort | egrep -v -e '^[^[:alnum:]]' -e '^$@$$' | xargs
