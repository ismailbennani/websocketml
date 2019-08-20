exception WSError of string
exception HTTPError of string
exception SyntaxError of string
exception NotImplemented of string

type opcode =
  | ContinuationFrame | TextFrame | BinaryFrame | Close | Ping | Pong
  | ControlFrame of int | NonControlFrame of int

let int_of_opcode = function
  | ContinuationFrame -> 0 | TextFrame -> 1 | BinaryFrame -> 2
  | Close -> 8 | Ping -> 9 | Pong -> 10
  | ControlFrame i | NonControlFrame i -> i

let opcode_of_int i =
       if i = 0  then ContinuationFrame
  else if i = 1  then TextFrame
  else if i = 2  then BinaryFrame
  else if i = 3  then NonControlFrame 3
  else if i = 4  then NonControlFrame 4
  else if i = 5  then NonControlFrame 5
  else if i = 6  then NonControlFrame 6
  else if i = 7  then NonControlFrame 7
  else if i = 8  then Close
  else if i = 9  then Ping
  else if i = 10 then Pong
  else if i = 11 then ControlFrame 11
  else if i = 12 then ControlFrame 12
  else if i = 13 then ControlFrame 13
  else if i = 14 then ControlFrame 14
  else if i = 15 then ControlFrame 15
  else raise (WSError ("Unknown op code " ^ (string_of_int i)))

(* cf. RFC6455 page 46 *)
type status_code =
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

exception Closed of status_code

let reserved_status_code = function
  | NormalClosure | GoingAway | ProtocolError | UnkownDatatype
  | InconsistentData | PolicyViolation | MsgTooBig | RequiredExtension
  | UnexpectedCondition -> false
  | NoStatusCode | AbnormalClosure | TLSFailure
  | ReservedCode _ | CustomCode _ -> true

let int_of_status_code = function
  | NormalClosure       -> 1000
  | GoingAway           -> 1001
  | ProtocolError       -> 1002
  | UnkownDatatype      -> 1003
  | NoStatusCode        -> 1005
  | AbnormalClosure     -> 1006
  | InconsistentData    -> 1007
  | PolicyViolation     -> 1008
  | MsgTooBig           -> 1009
  | RequiredExtension   -> 1010
  | UnexpectedCondition -> 1011
  | TLSFailure          -> 1015
  | ReservedCode i | CustomCode i -> i

let status_code_of_int i =
       if i = 1000 then NormalClosure
  else if i = 1001 then GoingAway
  else if i = 1002 then ProtocolError
  else if i = 1003 then UnkownDatatype
  else if i = 1005 then NoStatusCode
  else if i = 1006 then AbnormalClosure
  else if i = 1007 then InconsistentData
  else if i = 1008 then PolicyViolation
  else if i = 1009 then MsgTooBig
  else if i = 1010 then RequiredExtension
  else if i = 1011 then UnexpectedCondition
  else if i = 1015 then TLSFailure
  else if i >= 0 && i <= 999 ||
          i >= 1000 && i <= 2999 ||
          i >= 3000 && i <= 3999 then ReservedCode i
  else if i >= 4000 && i <= 4999 then CustomCode i
  else raise (WSError ("Unknown status code " ^ string_of_int i))

type frame = { fin : bool; opcode : opcode; frame_data : bytes}

type msg_type = BinaryMsg | TextMsg
type msg = { msg_typ : msg_type; msg_data : bytes }

type client = {
  addr : Unix.sockaddr;
  sock : Unix.file_descr
}
