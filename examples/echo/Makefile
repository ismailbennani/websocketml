include ../../config

all: echo_server.byte

echo_server.byte:
	$(OCAMLC) -o $@ unix.cma str.cma -I ../../lib websocketml.cma \
	echo_server.ml

clean:
	@rm -rf *.cm[iox]
	@rm -rf echo_server.byte
