# Authoritative build rules for tick.

# If you don't have GNU Make on your system, use this file as a
# cribsheet for how to build various aspects of tick.

STYLESDIR = .
STYLESHEET = juxt.css

.PHONY: 		watch default deploy test dev-docs-cljs docs/public/index.html

default:		docs/public/index.html

# Build the docs
docs/public/index.html:	docs/*.adoc docs/docinfo*.html ${STYLESDIR}/${STYLESHEET}
			asciidoctor -d book \
			-a "webfonts!" \
			-a stylesdir=../${STYLESDIR} \
			-a stylesheet=${STYLESHEET} \
			-D docs/public \
			-o $@ \
			docs/index.adoc

test-clj:
			./bin/kaocha :clj
test-chrome:
			rm -rf cljs-test-runner-out && mkdir -p cljs-test-runner-out/gen && clojure -Sverbose -M:test-chrome

test-node:
			rm -rf cljs-test-runner-out && mkdir -p cljs-test-runner-out/gen && clojure -Sverbose -M:test-node

test-all:
			make test-clj && make test-chrome && make test-node

# For developing the cljs used by the documentation, add --repl and change docs.cljs.edn optimizations to :none to develop interactively
dev-docs-cljs:
			clojure -M:docs-index

install:
			clojure -M:release install --version $(VERSION)
deploy:
			clojure -M:release --version $(VERSION)

# hooray for stackoverflow
.PHONY: list
list:
		@$(MAKE) -pRrq -f $(lastword $(MAKEFILE_LIST)) : 2>/dev/null | awk -v RS= -F: '/^# File/,/^# Finished Make data base/ {if ($$1 !~ "^[#.]") {print $$1}}' | sort | egrep -v -e '^[^[:alnum:]]' -e '^$@$$' | xargs
