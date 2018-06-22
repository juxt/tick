STYLESDIR = ../../asciidoctor-stylesheet-factory/stylesheets
STYLESHEET = juxt.css

index.html:	*.adoc ${STYLESDIR}/${STYLESHEET}
	asciidoctor -d book -a "webfonts!" -a stylesdir=${STYLESDIR} -a stylesheet=${STYLESHEET} index.adoc

index-default-style.html:	*.adoc
	asciidoctor -d book -a "webfonts!" index-default-style.adoc
