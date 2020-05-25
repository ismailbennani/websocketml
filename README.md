# websocketml
A simple websocket library for OCaml with no dependencies.

Documentation: http://www.ismailbennani.me/websocketml/Websocketml.html

## Install using `opam`

From this repo:
```
cd /path/to/websocketml
opam install .
```

## Simple example

Run a simple server
```
cd /path/to/websocketml
make
make examples
cd examples/echo
./echo_server.byte
```

Then open `examples/echo/echo_client.html` in your favorite browser (with javascript enabled)
