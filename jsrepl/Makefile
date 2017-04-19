OUTPUT_PATH=$(shell stack path --local-install-root)/bin/jsrepl.jsexe


build:
	mkdir -p "$(OUTPUT_PATH)"
	ln -nf "$(PWD)/index.html" "$(OUTPUT_PATH)/index.html"
	ln -nf "$(PWD)/looks.css" "$(OUTPUT_PATH)/looks.css"
	stack build
	echo Open $(OUTPUT_PATH)/index.html to see the file



