(* RFC6455 *)

open Bytes
open Utils

exception WSError of string
exception NotImplemented of string

(* ============================================ *)
(*                    OPCODE                    *)
(* ============================================ *)

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

let string_of_opcode = function
  | ContinuationFrame -> "ContinuationFrame" | TextFrame -> "TextFrame"
  | BinaryFrame -> "BinaryFrame" | Close -> "Close"
  | Ping -> "Ping" | Pong -> "Pong"
  | ControlFrame i -> "ControlFrame " ^ (string_of_int i)
  | NonControlFrame i -> "NonControlFrame " ^ (string_of_int i)

let is_controlop = function
  | ContinuationFrame | TextFrame | BinaryFrame | NonControlFrame _ -> false
  | Close | Ping | Pong | ControlFrame _ -> true

(* ============================================ *)
(*                 STATUS CODE                  *)
(* ============================================ *)

(* cf. RFC6455 page 46 *)
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

let reserved_exit_code = function
  | NormalClosure | GoingAway | ProtocolError | UnkownDatatype
  | InconsistentData | PolicyViolation | MsgTooBig | RequiredExtension
  | UnexpectedCondition -> false
  | NoStatusCode | AbnormalClosure | TLSFailure
  | ReservedCode _ | CustomCode _ -> true

let int_of_exit_code = function
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

let exit_code_of_int i =
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
  else raise (WSError ("Unknown exit code " ^ string_of_int i))

(* ============================================ *)
(*                   FRAME                      *)
(* ============================================ *)

type frame = { fin : bool; opcode : opcode; frame_data : bytes}

(* ============================================ *)
(*                    MSG                       *)
(* ============================================ *)

type msg_type = BinaryMsg | TextMsg
type msg = { msg_typ : msg_type; msg_data : bytes }

(* ============================================ *)
(*                   CLIENT                     *)
(* ============================================ *)

type client = {
  addr : Unix.sockaddr;
  sock : Unix.file_descr
}

let string_of_client c = string_of_sockaddr c.addr

(* ============================================ *)
(*                  WEBSOCKET                   *)
(* ============================================ *)

type t = {
  client : client;
  mutable closed_out : bool;
  mutable closed_in : bool;
}

let to_string sock = string_of_client sock.client
let print_t ff sock = Printf.fprintf ff "%s" (to_string sock)

let warn excptn =
  match excptn with
  | WSError s
  | NotImplemented s ->
    Printf.eprintf "Warning: %s\n" s
  | _ -> raise excptn

let create (sock, addr) = {
  client = { sock = sock;
             addr = addr; };
  closed_in = false;
  closed_out = false;
}

let get_sock sock = sock.client.sock
let get_addr sock = sock.client.addr
let closed_out sock = sock.closed_out
let closed_in sock = sock.closed_in
let closed sock = sock.closed_in || sock.closed_out

let recv_bytes sock rcv_buffer offset size =
  if closed_in sock then
    raise (WSError "WebSocket.recv_bytes : Connection is closed");
  let code = Unix.recv (get_sock sock) rcv_buffer offset size [] in
  code

let send_bytes sock b =
  if closed_out sock then
    raise (WSError "WebSocket.send_bytes : Connection is closed");
  Unix.send (get_sock sock) b 0 (Bytes.length b) []

let build_frame op data =
  let data_len = Bytes.length data in

  (* payload_len bytes *)
  let payload_len_additional_length, payload_len =
    if data_len < 126 then begin
      let res = Bytes.create 1 in
      set_uint8 res 0 data_len;
      0, res
    end else if data_len < 65536 then begin
      let res = Bytes.create 3 in
      set_uint8 res 0 126;
      set_uint16_be res 1 data_len;
      2, res
    end else begin
      let res = Bytes.create 9 in
      set_int8 res 0 127;
      set_int64_be res 1 (Int64.of_int data_len);
      8, res
    end
  in

  let length =
    1 + (* FIN | RSV 1/2/3 | OPCODE byte *)
    1 + (* MASK | PAYLOAD_LEN byte *)
    payload_len_additional_length + (* additional length *)
    data_len (* size of data *)
  in
  let msg = Bytes.create length in
  (* the server does not perform any fragmentation for now, the FIN
     flag is always set to 1 *)
  let first_byte = 128 + int_of_opcode op in
  set_uint8 msg 0 first_byte;
  (* the server does not mask its data, the MASK flag is always 0 *)
  Bytes.blit payload_len 0 msg 1 (1 + payload_len_additional_length);
  (* finally, we add the message *)
  Bytes.blit data 0 msg (2 + payload_len_additional_length) data_len;

  msg

(* TODO : handle fragmentation for big messages *)
let build_msg op data = build_frame op data
let send_msg sock op data =
  let msg = build_msg op data in
  Logger.debug (fun m -> m "Sending %s message with content\n\t\t%s"
                   (string_of_opcode op)
                   (Bytes.to_string data));
  send_bytes sock msg
let send_ping sock data = send_msg sock Ping data
let send_pong sock data = send_msg sock Pong data
let send_close sock data = send_msg sock Close data
let send_text sock msg = send_msg sock TextFrame (Bytes.of_string msg)
let send_binary sock data = send_msg sock BinaryFrame data

let close_with_message sock exit_code msg =
  let data =
    if reserved_exit_code exit_code then Bytes.empty
    else begin
      let exit_code_byte = Bytes.create 2 in
      set_uint16_be exit_code_byte 0 (int_of_exit_code exit_code);
      Bytes.cat exit_code_byte (Bytes.of_string msg)
    end
  in
  let code = send_close sock data in
  sock.closed_out <- true;
  code

let close sock exit_code = close_with_message sock exit_code ""

let do_close sock frame =
  (* optional exit code *)
  let code =
    if Bytes.length frame.frame_data >= 2 then
      get_uint16_be frame.frame_data 0
    else 1005
  in
  let reason =
    if Bytes.length frame.frame_data > 2 then
      Bytes.sub_string frame.frame_data 2 (Bytes.length frame.frame_data - 2)
    else ""
  in
  Logger.debug (fun m -> m "WebSocket connection closed with code %d and reason : %s" code reason);
  sock.closed_in <- true;
  close sock (exit_code_of_int code)

let receive_frame sock =
  Logger.debug (fun m -> m "%s" "Waiting for frame ...");

  let rcv_buffer = Bytes.create 2 in

  ignore (recv_bytes sock rcv_buffer 0 2);

  (* first byte is : FIN(1) RSV1(1) RSV2(1) RSV3(1) OPCODE(4)*)
  let first_byte = get_uint8 rcv_buffer 0 in

  let fin, opcode = split_first_bit first_byte in

  Logger.debug (fun m -> m "FIN : %s" (if fin then "1" else "0"));

  (* no extensions : RSV1/2/3 MUST be 0 *)
  if (opcode > 16) then
    warn (WSError
             ("No extension has been negotiated and RSV1/2/3 bits are not 0 (opcode = " ^
              (string_of_int opcode) ^ ")"));

  let opcode = opcode_of_int opcode in

  Logger.debug (fun m -> m "OPCODE: %d (%s)" (int_of_opcode opcode) (string_of_opcode opcode));

  (* second byte is : MASK(1) PAYLOAD_LEN(7) *)
  let second_byte = get_uint8 rcv_buffer 1 in
  let mask, payload_len = split_first_bit second_byte in

  Logger.debug (fun m -> m "MASK : %s" (if mask then "1" else "0"));
  Logger.debug (fun m -> m "PAYLOAD LEN : %d" payload_len);

  (* the sock MUST mask its message *)
  if not mask then raise (WSError "The sock frame is not masked");

  (* if payload len = 126, payload length (in bytes) is the next 2 bytes,
     if payload len = 127, payload length (in bytes) is the next 4 bytes
     else payload len is the payload length (in bytes)*)
  let payload_len =
    if payload_len = 126 then begin
      ignore (recv_bytes sock rcv_buffer 2 2);
      get_uint8 rcv_buffer 0
    end else if payload_len = 126 then begin
      ignore (recv_bytes sock rcv_buffer 2 8);
      Int64.to_int (get_int64_be rcv_buffer 0)
    end else payload_len
  in

  Logger.debug (fun m -> m "Real PAYLOAD LEN : %d" payload_len);

  (* get the masking key (32 bits) *)
  let masking_key = Bytes.create 4 in
  ignore (recv_bytes sock masking_key 0 4);

  Logger.debug (fun m -> m "MASKING KEY : 0x%02x 0x%02x 0x%02x 0x%02x"
                   (int_of_char (Bytes.get masking_key 0))
                   (int_of_char (Bytes.get masking_key 1))
                   (int_of_char (Bytes.get masking_key 2))
                   (int_of_char (Bytes.get masking_key 3)));

  let data =
    if payload_len > 0 then
      (* get the data *)
      let payload_data = Bytes.create payload_len in
      ignore (recv_bytes sock payload_data 0 payload_len);
      (* decode the data *)
      unmask_data masking_key payload_data
    else Bytes.empty
  in

  Logger.debug (fun m -> m "DATA : %s" (Bytes.to_string data));

  { fin; opcode; frame_data = data }

let receive_message sock =
  let msg_type = ref None in
  let msg_buffer = Buffer.create 16 in
  let rec aux first_frame =
    let frame = receive_frame sock in
    (* update msg_type and msg_buffer *)
    begin match frame.opcode with
      | ContinuationFrame ->
        if first_frame then
          raise (WSError "The opcode Continuation cannot be used in the first frame of a message");
        Buffer.add_bytes msg_buffer frame.frame_data
      | TextFrame ->
        if not first_frame then
          raise (WSError "The opcode TextFrame can only be used in the first frame of a message");
        msg_type := Some TextMsg;
        Buffer.add_bytes msg_buffer frame.frame_data
      | BinaryFrame ->
        if not first_frame then
          raise (WSError "The opcode BinaryFrame can only be used in the first frame of a message");
        msg_type := Some BinaryMsg;
        Buffer.add_bytes msg_buffer frame.frame_data
      | Close -> ignore (do_close sock frame)
      | Ping -> ignore (send_pong sock frame.frame_data)
      | Pong ->
        let string_data = Bytes.to_string frame.frame_data in
        Logger.debug (fun m -> m "%s" ("Received pong from sock with content : " ^ string_data))
      | ControlFrame i -> raise (NotImplemented ("Unknown opcode " ^ (string_of_int i)))
      | NonControlFrame i -> raise (NotImplemented ("Unknown opcode " ^ (string_of_int i)))
    end;

    (* if the connection has been closed, stop listening and return the message *)
    if closed_in sock then
      match !msg_type with
      | None -> None
      | Some typ ->
        Some { msg_typ = typ; msg_data = Buffer.to_bytes msg_buffer }
    else if is_controlop frame.opcode
    (* if the last frame was a control frame we do not change the value
       of first_frame, else it is no longer the first frame of the message *)
    then aux first_frame
    else if frame.fin
    (* if we are done, return the message *)
    then match !msg_type with
      | None -> assert false
      | Some typ ->
        Some { msg_typ = typ; msg_data = Buffer.to_bytes msg_buffer }
        (* else continue reading new frames *)
    else aux false
  in aux true
