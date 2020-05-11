type t

val create : Unix.sockaddr -> t
val listen : t -> unit
val accept : t -> Unix.file_descr * Unix.sockaddr
val listen_and_accept : t -> Unix.file_descr * Unix.sockaddr
val close : t -> unit
val do_ws_handshake : Unix.file_descr -> unit
