let _ =
  let tests = [
    (* decoded, encoded *)
    "", "";
    "f", "Zg==";
    "fo", "Zm8=";
    "foo", "Zm9v";
    "foob", "Zm9vYg==";
    "fooba", "Zm9vYmE=";
    "foobar", "Zm9vYmFy";
  ] in
  print_endline "Test encoding: ";
  let ok_enc = List.fold_left (fun ok (decoded, encoded) ->
      let res = Base64.encode decoded in
      let test_ok = res = encoded in
      Printf.printf "BASE64(\"%s\") = \"%s\" (expected \"%s\") %s\n"
        decoded res encoded (if test_ok then "OK" else "NOT OK");
      ok && test_ok
    ) true tests
  in
  print_newline ();

  print_endline "Test decoding: ";
  let ok_dec = List.fold_left (fun ok (decoded, encoded) ->
      let res = Base64.decode encoded in
      let test_ok = res = decoded in
      Printf.printf "BASE64⁻¹(\"%s\") = \"%s\" (expected \"%s\") %s\n"
        encoded res decoded (if res = decoded then "OK" else "NOT OK");
      ok && test_ok
    ) true tests
  in
  print_newline ();

  if not ok_enc || not ok_dec then exit 1;
