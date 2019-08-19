(* SHA1 encoding defined in RFC3174 *)

exception Sha1Error of string

open Bytes

let show_byte ff b =
  Printf.fprintf ff "%s" (Utils.hex_string_of_bytes b)

let show_intarray ff a =
  Printf.fprintf ff "%s" "\t\t";
  Array.iteri (fun i v ->
      Printf.fprintf ff "%08x%s" v (if (i + 1) mod 10 = 0 then "\n\t\t" else " ")
    ) a

let show_int32array ff a =
  Printf.fprintf ff "%s" "\t\t";
  Array.iteri (fun i v ->
      let v = Int32.to_int v in
      Printf.fprintf ff "%08x%s" v (if (i + 1) mod 10 = 0 then "\n\t\t" else " ")
    ) a

let bytes_of_h (h0, h1, h2, h3, h4) =
  let res = Bytes.create 20 in
  set_int32_be res 0 h0;
  set_int32_be res 4 h1;
  set_int32_be res 8 h2;
  set_int32_be res 12 h3;
  set_int32_be res 16 h4;
  res

let read_int32 b i = get_int32_be b i

let ( +++ ) a b : int32 = Int32.add a b
let circular_left_shift n x : int32 =
  Int32.logor (Int32.shift_left x n)
              (Int32.shift_right_logical x (32-n))

let f t b c d : int32 =
  if t >=  0 && t <= 19 then Int32.logor (Int32.logand b c) (Int32.logand (Int32.lognot b) d)
  else if t >= 20 && t <= 39 then Int32.logxor b (Int32.logxor c d)
  else if t >= 40 && t <= 59 then Int32.logor (Int32.logand b c) (Int32.logor (Int32.logand b d) (Int32.logand c d))
  else if t >= 60 && t <= 79 then Int32.logxor b (Int32.logxor c d)
  else raise (Sha1Error ("f(t,B,C,D) : Unvalid argument t = " ^ (string_of_int t)))

let k t : int32 =
  if t >=  0 && t <= 19 then Int32.of_int 0x5A827999
  else if t >= 20 && t <= 39 then Int32.of_int 0x6ED9EBA1
  else if t >= 40 && t <= 59 then Int32.of_int 0x8F1BBCDC
  else if t >= 60 && t <= 79 then Int32.of_int 0xCA62C1D6
  else raise (Sha1Error ("k(t) : Unvalid argument t = " ^ (string_of_int t)))

let pad s =
  let original_length = Bytes.length s in
  let n_blocks = truncate (ceil ((float original_length) /. 64.)) in
  let n_blocks =
    (* we need enough space to add a bit of 1 (it will be the byte 0x80 since
       the message is a string so its length in bits is a multiple of 8)
       and two bytes for the length of the string *)
    if original_length mod 64 > 0 && original_length mod 64 < 52
    then n_blocks
    else n_blocks + 1
  in

  let res = Bytes.make (n_blocks * 64) (char_of_int 0) in

  (* start by copying the message *)
  Bytes.blit s 0 res 0 original_length;

  (* write 1 after the message (here we write the byte 0b10000000) *)
  Bytes.set res original_length (char_of_int 0x80);

  (* write the original length of the message as a 2 bytes integer at the end *)
  (* NOTE : this is taken from the JS code found in
      www.movable-type.co.uk/scripts/sha1.html, I do not yet understand
      why the length is shifted 3 times to the left *)
  let length_1 = (original_length lsl 3) lsr 32 in
  let length_2 = (original_length lsl 3) land 0xFFFFFFFF in
  set_int32_be res (n_blocks * 64 - 8) (Int32.of_int length_1);
  set_int32_be res (n_blocks * 64 - 4) (Int32.of_int length_2);

  res

let digest_block (h0, h1, h2, h3, h4) block =
  (* block is a 512 bits block *)
  (* let w = Bytes.create (80 * 4) in *)
  let w = Array.make 80 Int32.zero in

  (* 16 first loops : Divide block into 16 words W(0), W(1), ... , W(15) *)
  for i = 0 to 15 do
    w.(i) <- read_int32 block (4 * i);
  done;

  for i = 16 to 79 do
    let aux = Int32.logxor (Int32.logxor w.(i-3) w.(i-8))
        (Int32.logxor w.(i-14) w.(i-16)) in
    w.(i) <- (circular_left_shift 1 aux)
  done;

  let a = ref h0 and b = ref h1 and c = ref h2 and d = ref h3 and e = ref h4 in

  for i = 0 to 79 do
    let tmp = (circular_left_shift 5 !a) +++ (f i !b !c !d) +++ !e +++
              (w.(i)) +++ (k i) in
    e := !d; d := !c;
    c := circular_left_shift 30 !b;
    b := !a; a := tmp
  done;

  let new_h = h0 +++ !a, h1 +++ !b, h2 +++ !c, h3 +++ !d, h4 +++ !e in
  new_h

let hash s =
  let s = pad (Bytes.of_string s) in
  let s_length = Bytes.length s in
  let rec step h start =
    if start >= s_length then
      Bytes.to_string (bytes_of_h h)
    else
      let new_h = digest_block h (Bytes.sub s start 64) in
      step new_h (start + 64)
  in step (Int32.of_int 0x67452301,
           Int32.of_int 0xEFCDAB89,
           Int32.of_int 0x98BADCFE,
           Int32.of_int 0x10325476,
           Int32.of_int 0xC3D2E1F0)
    0
