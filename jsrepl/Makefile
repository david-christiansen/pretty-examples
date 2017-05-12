BIN_PATH=$(shell stack path --local-install-root)/bin
OUTPUT_PATH1=$(shell stack path --local-install-root)/bin/jsrepl.jsexe
OUTPUT_PATH2=$(shell stack path --local-install-root)/bin/ppupload.jsexe


build:
	mkdir -p "$(OUTPUT_PATH1)"
	ln -nf "$(PWD)/index.html" "$(OUTPUT_PATH1)/index.html"
	ln -nf "$(PWD)/looks.css" "$(OUTPUT_PATH1)/looks.css"
	mkdir -p "$(OUTPUT_PATH2)"
	ln -nf "$(PWD)/index.html" "$(OUTPUT_PATH2)/index.html"
	ln -nf "$(PWD)/looks.css" "$(OUTPUT_PATH2)/looks.css"
	stack build
	echo Open $(OUTPUT_PATH1)/index.html to see the file
	echo Open $(OUTPUT_PATH2)/index.html to see the file

demodist: build
	$(eval WHEN := $(shell date +'%Y-%m-%d-%H-%M-%S'))
	tar czf ppdemo1-$(WHEN).tar.gz -C $(BIN_PATH) jsrepl.jsexe
	tar czf ppdemo2-$(WHEN).tar.gz -C $(BIN_PATH) ppupload.jsexe

