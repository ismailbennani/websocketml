include Ws

type http_t = Http.t

let http_create = Http.create
let http_listen = Http.listen
let http_accept = Http.accept
let http_listen_and_accept = Http.listen_and_accept
let http_close = Http.close
let http_do_ws_handshake = Http.do_ws_handshake
