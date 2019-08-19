.PHONY: lib examples test

all: lib examples

install: lib
	mkdir -p /usr/local/lib/websocketml/
	cp lib/* /usr/local/lib/websocketml/

lib:
	$(MAKE) -C src
	cp src/*.cmi src/*.cma lib

examples: lib
	$(MAKE) -C examples

test: lib
	$(MAKE) -C test run

clean cleanall realclean:
	$(MAKE) -C src clean
	$(MAKE) -C examples clean
	$(MAKE) -C test clean
