.PHONY: examples test

all: lib examples

lib:
	mkdir -p lib
	$(MAKE) -C src
	cp src/*.cmi src/*.cma lib

examples: lib
	$(MAKE) -C examples

test: lib
	$(MAKE) -C test run

clean cleanall realclean:
	rm -rf lib
	$(MAKE) -C src clean
	$(MAKE) -C examples clean
	$(MAKE) -C test clean
