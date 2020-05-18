(** This library implements {{:https://tools.ietf.org/html/rfc6455}RFC6455}.
    There are currently no supported extensions.
*)

(** {1 WS} *)

exception WSError of string

type t

type msg_type = BinaryMsg | TextMsg
type msg = { msg_typ : msg_type; msg_data : bytes }

(** All these op codes are defined in the RFC *)
type opcode =
  | ContinuationFrame | TextFrame | BinaryFrame | Close | Ping | Pong
  | ControlFrame of int | NonControlFrame of int

(** All these exit codes are defined in the RFC *)
type exit_code =
  | NormalClosure       (** 1000 *)
  | GoingAway           (** 1001 *)
  | ProtocolError       (** 1002 *)
  | UnkownDatatype      (** 1003 *)
  | NoStatusCode        (** 1005 *)
  | AbnormalClosure     (** 1006 *)
  | InconsistentData    (** 1007 *)
  | PolicyViolation     (** 1008 *)
  | MsgTooBig           (** 1009 *)
  | RequiredExtension   (** 1010 *)
  | UnexpectedCondition (** 1011 *)
  | TLSFailure          (** 1015 *)
  | ReservedCode of int (** ranges :
                            0 - 999 : not used
                            1000 - 2999 : reserved for websocket protocol
                            3000 - 3999 : reserved for public libraries *)
  | CustomCode of int   (** range 4000 - 4999 : private use *)

(** Create a new connexion.

    WARNING: this method assumes that the socket is already open and that
             the WebSocket handshake has already been performed. Refer to
             {!Http.create}, {!Http.listen_and_accept} and
             {!Http.do_ws_handshake} methods of module {!Http}.
*)
val create : Unix.file_descr * Unix.sockaddr -> t

(** Get the unix socket *)
val get_sock       : t -> Unix.file_descr

(** Get remote address *)
val get_addr       : t -> Unix.sockaddr

(** {2 Read} *)

(** Block until a complete message is received. This method may read several
    frames in a single call until it reaches the final frame of the message.

    If the received frame is a Close frame, it closes the connection and returns
    None.

    If the received frame is a Ping, it answers with an appropriate Pong and
    returns None.

    If the received frame is a Pong, it outputs a debug message and returns
    None.

    Raise {!WSError} if the socket is closed or if the received message is
    ill-formed.
*)
val receive_message : t -> msg option

(** {2 Write} *)
(** These method may raise {!WSError} if the socket is closed *)

val send_msg : t -> opcode -> bytes -> int
val send_ping : t -> bytes -> int
val send_text : t -> string -> int
val send_binary : t -> bytes -> int
val close : t -> exit_code -> int
val close_with_message : t -> exit_code -> string -> int

(** {2 Helpers} *)

(** Return true if the socket cannot be read from *)
val closed_in  : t -> bool

(** Return true if the socket cannot be written to *)
val closed_out : t -> bool

(** Return true if the socket cannot be read from or written to *)
val closed     : t -> bool

val to_string : t -> string
val print_t : out_channel -> t -> unit

(** {1 HTTP} *)

(** Implement a subset of HTTP/1.1 {{:https://tools.ietf.org/html/rfc2616}RFC2616}.
    Its only purpose is to do a websocket handshake, it cannot be used as
    regular HTTP server. One could use any other mean to retrieve an open
    socket and client address to use as inputs of {!Websocketml.create}.
*)
module Http :
sig
  exception HTTPError of string

  type t

  (** Create a TCP socket and bind it to given address *)
  val create : Unix.sockaddr -> t

  (** Set up the socket for receiving connection requests
      (with a pending list of size 1) *)
  val listen : t -> unit

  (** Block until a connection is received and return the socket and address
      of the connecting client *)
  val accept : t -> Unix.file_descr * Unix.sockaddr

  (** Call listen then accept *)
  val listen_and_accept : t -> Unix.file_descr * Unix.sockaddr

  (** Read a WebSocket opening request on the given socket and answer with
      an appropriate message to complete the handshake defined
      by {{:https://tools.ietf.org/html/rfc6455}RFC6455}.

      Raise {!HTTPError} if the received request is not a valid HTTP request or
      if it is not a valid WebSocket opening request.
  *)
  val do_ws_handshake : Unix.file_descr -> unit

  (** Close the server's TCP socket *)
  val close : t -> unit

  val to_string : t -> string
  val print_t : out_channel -> t -> unit
end

(** {1 Logger} *)

(** usage example: {v Logger.info (fun f -> f "[%s] %d" some_string some_int) v} *)
module Logger :
sig
  type verbose = ERROR | WARN | INFO | DEBUG
  val set_verbose : verbose -> unit

  val error : ((('a, out_channel, unit) format -> 'a) -> 'b) -> unit
  val warn : ((('a, out_channel, unit) format -> 'a) -> 'b) -> unit
  val info : ((('a, out_channel, unit) format -> 'a) -> 'b) -> unit
  val debug : ((('a, out_channel, unit) format -> 'a) -> 'b) -> unit
end
