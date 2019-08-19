let _ =

  let int32_of_hex_string s =
    Int32.of_int (int_of_string ("0x" ^ s))
  in

  let bytes_of_hex_string r =
    let hex_strings = String.split_on_char ' ' r in
    let hex_ints = List.map int32_of_hex_string hex_strings in
    let res = Bytes.create (List.length hex_ints * 4) in
    List.iteri (fun i v -> Bytes.set_int32_be res (4*i) v) hex_ints;
    res
  in

  let run_test clear hashed =
    let res = Bytes.of_string (Sha1.hash clear) in
    let expected = bytes_of_hex_string hashed in
    Printf.printf "SHA1(\"%s\") =\n%a\nexpected\n%a\n"
      clear Sha1.show_byte res Sha1.show_byte expected;
    res = expected
  in

  let ok = run_test "" "da39a3ee 5e6b4b0d 3255bfef 95601890 afd80709" in

  print_endline (if ok then "OK\n" else "NOT OK\n");

  let ok = ok &&
           run_test "abc" "a9993e36 4706816a ba3e2571 7850c26c 9cd0d89d" in

  print_endline (if ok then "OK\n" else "NOT OK\n");

  let ok = ok &&
           run_test "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
             "84983e44 1c3bd26e baae4aa1 f95129e5 e54670f1" in

  print_endline (if ok then "OK\n" else "NOT OK\n");

  let ok = ok &&
           run_test "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
             "a49b2446 a02c645b f419f995 b6709125 3a04a259" in

  print_endline (if ok then "OK\n" else "NOT OK\n");

  (* let ok = ok &&
           run_test (String.make 1000000 'a')
             "34aa973c d4c4daa4 f61eeb2b dbad2731 6534016f" in

  print_endline (if ok then "OK\n" else "NOT OK\n"); *)

  (* pass : trust me. If you don't, take an 8 min coffee brake and run it
     (disable the printing of the decoded string before) *)
  (* let ok = ok &&
           run_test
             (* generate a 1GB string *)
             (String.init (16777216*64)
                (fun i ->
                   String.get
                     "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno"
                     (i mod 64)))
             "7789f0c9 ef7bfc40 d9331114 3dfbe69e 2017f592" in *)

  if not ok then exit 1
