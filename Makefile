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
			lein doo node test

# For developing the cljs used by the documentation, uses shadow-cljs
# See shadow-cljs.edn for configuration
dev-docs-cljs:
			npm i; shadow-cljs watch doc bootstrap-support

pom.xml:
			clj -Spom

deploy:			pom.xml
			mvn deploy
