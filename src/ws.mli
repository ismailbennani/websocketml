type msg_type = BinaryMsg | TextMsg
type msg = { msg_typ : msg_type; msg_data : bytes }

type opcode =
  | ContinuationFrame | TextFrame | BinaryFrame | Close | Ping | Pong
  | ControlFrame of int | NonControlFrame of int

type exit_code =
  (* These are defined in the RFC *)
  | NormalClosure       (* 1000 *)
  | GoingAway           (* 1001 *)
  | ProtocolError       (* 1002 *)
  | UnkownDatatype      (* 1003 *)
  | NoStatusCode        (* 1005 *)
  | AbnormalClosure     (* 1006 *)
  | InconsistentData    (* 1007 *)
  | PolicyViolation     (* 1008 *)
  | MsgTooBig           (* 1009 *)
  | RequiredExtension   (* 1010 *)
  | UnexpectedCondition (* 1011 *)
  | TLSFailure          (* 1015 *)
  | ReservedCode of int (* ranges :
                            0 - 999 : not used
                            1000 - 2999 : reserved for websocket protocol
                            3000 - 3999 : reserved for public libraries *)
  | CustomCode of int   (* range 4000 - 4999 : private use *)

type t

val to_string : t -> string
val print_t : out_channel -> t -> unit

val get_sock       : t -> Unix.file_descr
val get_addr       : t -> Unix.sockaddr
val closed     : t -> bool
val closed_in  : t -> bool
val closed_out : t -> bool

val create : Unix.file_descr -> Unix.sockaddr -> t
val receive_message : t -> msg option
val send_msg : t -> opcode -> bytes -> int
val send_ping : t -> bytes -> int
val send_text : t -> string -> int
val send_binary : t -> bytes -> int
val close : t -> exit_code -> int
