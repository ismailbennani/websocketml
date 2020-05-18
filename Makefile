include config

.PHONY: lib examples test

all: lib

config:
	./configure

install: lib
	mkdir -p $(LIBDIR)
	cp lib/* $(LIBDIR)
	$(OCAMLFIND) install fadbadml META

lib: config
	$(MAKE) -C src
	mkdir -p lib
	cp src/*.cmi src/*.cma lib

doc: lib
	mkdir -p docs
	ocamldoc $(OCAMLFLAGS) -html -d docs -css-style css/doc_style.css -verbose \
		-hide Stdlib,Websocketml.Http,Ws,Sha1,Base64,Utils \
		-t websocketml -show-missed-crossref -charset utf8 -short-functors \
		-short-paths \
		-I src \
		src/websocketml.mli src/websocketml.ml

examples: lib
	$(MAKE) -C examples

test: lib
	$(MAKE) -C test run

clean:
	$(MAKE) -C src clean
	$(MAKE) -C examples clean
	$(MAKE) -C test clean

cleanall realclean mrproper: clean
	$(MAKE) -C src cleanall
	$(MAKE) -C examples cleanall
	$(MAKE) -C test cleanall
	rm -rf lib
	rm -f config META opam
