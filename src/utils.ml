open Types

let hex_string_of_bytes b =
  let res = ref ["\t\t"] in
  for i = 0 to Bytes.length b - 1 do
    res := (Printf.sprintf "0x%02x%s"
              (int_of_char (Bytes.get b i))
              (if i mod 20 = 19 then "\n\t\t" else " "))
           :: !res
  done;
  String.concat "" (List.rev !res)

let string_of_opcode = function
  | ContinuationFrame -> "ContinuationFrame" | TextFrame -> "TextFrame"
  | BinaryFrame -> "BinaryFrame" | Close -> "Close"
  | Ping -> "Ping" | Pong -> "Pong"
  | ControlFrame i -> "ControlFrame " ^ (string_of_int i)
  | NonControlFrame i -> "NonControlFrame " ^ (string_of_int i)

let string_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (a,p) -> (Unix.string_of_inet_addr a) ^ ":" ^ (string_of_int p)

let string_of_client c = string_of_sockaddr c.addr

(* b is a uint8 *)
let split_first_bit b =
  let first_bit = b lsr 7 in
  (first_bit = 1), b land 0b01111111

(* decode data in-place *)
let unmask_data mask data =
  let char_xor (b1 : char) (b2 : char) =
    char_of_int ((int_of_char b1) lxor (int_of_char b2))
  in
  for i = 0 to Bytes.length data - 1 do
    Bytes.set data i (char_xor (Bytes.get data i) (Bytes.get mask (i mod 4)));
  done;
  data

let is_controlop = function
  | ContinuationFrame | TextFrame | BinaryFrame | NonControlFrame _ -> false
  | Close | Ping | Pong | ControlFrame _ -> true

let apply_opt f = function
  | None -> None
  | Some x -> Some (f x)
