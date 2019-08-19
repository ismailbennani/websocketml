include config

.PHONY: lib examples test

all: lib examples

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

clean cleanall realclean:
	$(MAKE) -C src clean
	$(MAKE) -C examples clean
	$(MAKE) -C test clean
	rm -f config META opam
