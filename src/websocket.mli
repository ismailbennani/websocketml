type t

val to_string : t -> string

val get_sock       : t -> Unix.file_descr
val get_addr       : t -> Unix.sockaddr
val closed     : t -> bool
val closed_in  : t -> bool
val closed_out : t -> bool

val create : Unix.file_descr -> Unix.sockaddr -> t
val receive_message : t -> Types.msg option
val send_msg : t -> Types.opcode -> bytes -> int
val send_ping : t -> bytes -> int
val send_text : t -> string -> int
val send_binary : t -> bytes -> int
val close : t -> Types.status_code -> int
