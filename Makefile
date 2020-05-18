include config

.PHONY: lib examples test

all: lib

config:
	./configure

install: lib
	mkdir -p $(LIBDIR)
	cp lib/* $(LIBDIR)

lib: config
	$(MAKE) -C src
	mkdir -p lib
	cp src/*.cmi src/*.cma lib

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
