STYLESDIR = ../asciidoctor-stylesheet-factory/stylesheets
STYLESHEET = juxt.css

.PHONY: 		watch default

default:		docs/index.html

# Build the docs
docs/index.html:	docs/*.adoc docs/docinfo*.html ${STYLESDIR}/${STYLESHEET}
			asciidoctor -d book \
			-a "webfonts!" \
			-a stylesdir=../${STYLESDIR} \
			-a stylesheet=${STYLESHEET} \
			docs/index.adoc

# For developing the cljs used by the documentation, uses shadow-cljs
# See shadow-cljs.edn for configuration
watch:
			shadow-cljs watch doc bootstrap-support
