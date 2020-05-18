# websocketml
A simple websocket library for OCaml with no dependencies

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

## Install using `opam`

```
cd /path/to/websocketml
opam install .
```

## Build documentation

```
cd /path/to/websocketml
make
make doc
```

Then open `docs/index.html`.
