# Authoritative build rules for tick.

# If you don't have GNU Make on your system, use this file as a
# cribsheet for how to build various aspects of tick.

STYLESDIR = ../asciidoctor-stylesheet-factory/stylesheets
STYLESHEET = juxt.css

.PHONY: 		watch default deploy test dev-doc-cljs

default:		docs/index.html

# Build the docs
docs/index.html:	docs/*.adoc docs/docinfo*.html ${STYLESDIR}/${STYLESHEET}
			asciidoctor -d book \
			-a "webfonts!" \
			-a stylesdir=../${STYLESDIR} \
			-a stylesheet=${STYLESHEET} \
			docs/index.adoc

test:
			clj -Atest -e deprecated
			clj -Rtest -Ctest -m cljs-test-runner.main

# For developing the cljs used by the documentation, uses shadow-cljs
# See shadow-cljs.edn for configuration
dev-docs-cljs:
			npm i; shadow-cljs watch doc bootstrap-support

pom.xml:
			clj -Spom
# Dev pom is used to created development project with intellij
dev-pom:
			clj -R:dev:dev/rebel:dev/nrepl -C:dev:dev/rebel:dev/nrepl -Spom
			

deploy:			pom.xml
			mvn deploy
figwheel:
			clj -R:dev:dev/nrepl:dev/rebel -C:dev:dev/nrepl:dev/rebel:test -m figwheel.main --build tick --repl

