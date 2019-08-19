(* BASE64 encoding and decoding defined in
   RFC4648 [https://tools.ietf.org/html/rfc4648] *)

exception Base64Error of string

(* 64 characters + padding value (=) *)
let alphabet =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="

let get_letter i =
  if i > 64 then
    raise (Base64Error ("No letter number " ^
                        (string_of_int i) ^ " in base64 alphabet"));
  String.get alphabet i

let encode s =
  let get_at i = int_of_char (String.get s i) in

  let encode_3 i1 i2 i3 =
    let c1 = i1 lsr 2 in
    let c2 = ((i1 land 0b00000011) lsl 4) lor (i2 lsr 4) in
    let c3 = ((i2 land 0b00001111) lsl 2) lor (i3 lsr 6) in
    let c4 = i3 land 0b00111111 in
    c1, c2, c3, c4
  in

  let encode_2 i1 i2 =
    let c1, c2, c3, _ = encode_3 i1 i2 0 in
    c1, c2, c3, 64
  in

  let encode_1 i1 =
    let c1, c2, _, _ = encode_2 i1 0 in
    c1, c2, 64, 64
  in

  let s_length = String.length s in

  let rec encode_step acc start_read start_write =
    if start_read = s_length then
      Bytes.to_string acc
    else
      let (c1, c2, c3, c4), new_start_read =
        if s_length - start_read >= 3 then
          encode_3 (get_at start_read) (get_at (start_read+1))
            (get_at (start_read+2)),
          start_read + 3
        else if s_length - start_read = 2 then
          encode_2 (get_at start_read) (get_at (start_read+1)),
          start_read + 2
        else if s_length - start_read = 1 then
          encode_1 (get_at start_read),
          start_read + 1
        else assert false
      in

      Bytes.set acc (start_write) (get_letter c1);
      Bytes.set acc (start_write + 1) (get_letter c2);
      Bytes.set acc (start_write + 2) (get_letter c3);
      Bytes.set acc (start_write + 3) (get_letter c4);

      encode_step acc new_start_read (start_write + 4)
  in

  let encoded_size = truncate (ceil ((float s_length) /. 3.)) * 4 in
  encode_step (Bytes.create encoded_size) 0 0

let decode s =
  let get_at i = String.index alphabet (String.get s i) in

  let decode_4 c1 c2 c3 c4 =
    let i1 = (c1 lsl 2) lor (c2 lsr 4) in
    let i2 = ((c2 land 0b001111) lsl 4) lor (c3 lsr 2) in
    let i3 = ((c3 land 0b000011) lsl 6) lor c4 in
    i1, i2, i3
  in

  let decode_3 c1 c2 c3 =
    let i1, i2, _ = decode_4 c1 c2 c3 0 in
    i1, i2, -1
  in

  let decode_2 c1 c2 =
    let i1, _, _ = decode_3 c1 c2 0 in
    i1, -1, -1
  in

  let s_length = String.length s in

  if s_length mod 4 <> 0 then
    raise (Base64Error ("decode: unvalid string of size " ^
                        (string_of_int s_length) ^
                        ", the size of a base64 encoded string " ^
                        "should be a multiple of 4"));

  let rec decode_step acc start_read start_write =
    if start_read = s_length then
      Bytes.to_string acc
    else
      let i1, i2, i3 =
        if String.get s (start_read + 3) = '=' then
          if String.get s (start_read + 2) = '=' then
            decode_2 (get_at start_read) (get_at (start_read + 1))
          else
            decode_3 (get_at start_read) (get_at (start_read + 1))
              (get_at (start_read + 2))
        else
            decode_4 (get_at start_read) (get_at (start_read + 1))
              (get_at (start_read + 2)) (get_at (start_read + 3))
      in

      Bytes.set acc (start_write) (char_of_int i1);
      if i2 <> -1 then Bytes.set acc (start_write + 1) (char_of_int i2);
      if i3 <> -1 then Bytes.set acc (start_write + 2) (char_of_int i3);


      decode_step acc (start_read + 4) (start_write + 3)
  in

  let decoded_size = s_length * 3 / 4 in
  let decoded_size =
    if s_length > 0 then
      let decoded_size =
        if String.get s (s_length - 1) = '='
        then decoded_size - 1
        else decoded_size
      in
      let decoded_size =
        if String.get s (s_length - 2) = '='
        then decoded_size - 1
        else decoded_size
      in
      decoded_size
    else decoded_size
  in

  decode_step (Bytes.create decoded_size) 0 0
