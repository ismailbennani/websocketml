(* This is a very simple HTTP/1.1 server implementation [RFC2616] *)

open Unix
open Utils
open Types

let print_address ff sockaddr =
  let addr =
    match sockaddr with
    | ADDR_UNIX s -> s
    | ADDR_INET (inet, port) ->
      (string_of_inet_addr inet) ^ ":" ^ (string_of_int port)
  in
  Printf.fprintf ff "%s" addr


module FieldMap = Map.Make(String)

let ws_uuid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
let build_server_key client_key =
  Logger.debug (fun m -> m "Got client key \"%s\"" client_key);
  let key = client_key ^ ws_uuid in
  Logger.debug (fun m -> m "Built key \"%s\"" key);
  let sha_hashed = Sha1.hash key in
  let hex_string =
    let inp = Bytes.of_string sha_hashed in
    let res = ref "" in
    for i = 0 to Bytes.length inp - 1 do
      res := !res ^ (Printf.sprintf "%02x" (int_of_char (Bytes.get inp i)))
    done;
    !res
  in
  Logger.debug (fun m -> m "SHA1 hashed to 0x%s = \"%s\"" hex_string sha_hashed);
  let base64_encoded = Base64.encode sha_hashed in
  Logger.debug (fun m -> m "Base64 encoded to \"%s\"" base64_encoded);
  base64_encoded

(* *)

let accepted_fields =
  [| (* as defined in the RFC, with no extension *)
    (* general-header, page 35 *)
    "Cache-Control"; "Connection"; "Date"; "Pragma"; "Trailer";
    "Transfer-Encoding"; "Upgrade"; "Via"; "Warning";
    (* request-header, page 38-39 *)
    "Accept"; "Accept-Charset"; "Accept-Encoding"; "Accept-Language";
    "Authorization"; "Expect"; "From"; "Host"; "If-Match";
    "If-Modified-Since"; "If-None-Match"; "If-Range"; "If-Unmodified-Since";
    "Max-Forwards"; "Proxy-Authorization"; "Range"; "Referer"; "TE";
    "User-Agent";
    (* entity-header, page 42 *)
    "Allow"; "Content-Encoding"; "Content-Language"; "Content-Length";
    "Content-Location"; "Content-MD5"; "Content-Range"; "Content-Type";
    "Expires"; "Last-Modified";
    (* extension-header *)
    "Origin"; "Cookie";
    "Sec-WebSocket-Key"; "Sec-WebSocket-Version"; "Sec-WebSocket-Protocol";
    "Sec-WebSocket-Extensions";
  |]

let add_field field_map field value =
  if not (Array.mem field accepted_fields) then
    Logger.warn (fun m -> m "Unknown field type %s (ignored)" field);
  FieldMap.add field value field_map

(* *)

type method_ty =
  (* as defined page 36*)
  OPTIONS | GET | HEAD | POST | PUT | DELETE | TRACE | CONNECT
  (* | extension-method no extension implemented yet *)

let method_ty_of_string s =
  let s = String.uppercase_ascii s in
  if s = "OPTIONS" then OPTIONS
  else if s = "GET" then GET
  else if s = "HEAD" then HEAD
  else if s = "POST" then POST
  else if s = "PUT" then PUT
  else if s = "DELETE" then DELETE
  else if s = "TRACE" then TRACE
  else if s = "CONNECT" then CONNECT
  else raise (HTTPError ("Method " ^ s ^ " is not a valid HTTP/1.1 method"))

let string_of_method_ty = function
    OPTIONS -> "OPTIONS" | GET -> "GET" | HEAD -> "HEAD"
  | POST -> "POST" | PUT -> "PUT" | DELETE -> "DELETE"
  | TRACE -> "TRACE" | CONNECT -> "CONNECT"

(* *)

type request = {
  typ : method_ty;
  uri : string;
  fields : string list FieldMap.t;
  content : string option
}

let typ req = req.typ
let uri req = req.uri
let fields req = req.fields
let content req = req.content

let print_request ff req =
  let print_method_ty ff typ =
    Printf.fprintf ff "%s" (string_of_method_ty typ)
  in
  let print_values ff values =
    Printf.fprintf ff "%s" (String.concat ", " values)
  in
  let print_field ff (field, values) =
    Printf.fprintf ff "%s : %a" field print_values values
  in
  let print_fields ff fields =
    FieldMap.iter
      (fun field values -> Printf.fprintf ff "%a\n" print_field (field, values))
      fields
  in
  let print_content ff = function
    | None -> ()
    | Some s -> Printf.fprintf ff "%s" s
  in
  Printf.fprintf ff "%a %s HTTP/1.1\n%a\n\n%a"
    print_method_ty (typ req) (uri req) print_fields (fields req)
    print_content (content req)

(* *)

(* server type, it corresponds to the root location *)
type t = {
  addr : Unix.sockaddr;
  sock : Unix.file_descr;
}

let addr serv = serv.addr
let sock serv = serv.sock

(* *)

let parse_request req =
  (* split header and content *)
  let aux = Str.bounded_split_delim (Str.regexp "\n\n") req 2 in

  let header = List.nth aux 0 in
  let content = List.nth_opt aux 1 in

  (* split request line and field lines *)
  let header_lines = String.split_on_char '\n' header in
  let request_line = List.hd header_lines in
  let field_lines = List.tl header_lines in

  (* parse request line *)
  let aux = Str.split (Str.regexp "[ \t]") request_line in
  let method_ty = String.trim (List.hd aux) in
  let request_uri = String.trim (List.hd (List.tl aux)) in
  let protocol = String.trim (List.hd (List.tl (List.tl aux))) in

  (* check if protocol is HTTP/1.1 *)
  let protocol = String.uppercase_ascii protocol in
  if protocol <> "HTTP/1.1" then
    raise (HTTPError ("Unknown protocol " ^ protocol ^ ". Supported protocols are : HTTP/1.1"));

  (* check and get method type *)
  let method_ty = method_ty_of_string method_ty in

  (* parse field lines *)
  let field_map = List.fold_left (fun field_map field_line ->
      if String.trim field_line = "" then field_map
      else
        let aux = Str.bounded_split (Str.regexp ":") field_line 2 in
        if List.length aux <> 2 then
          raise (SyntaxError field_line);
        let field = String.trim (List.hd aux) in
        let values = List.hd (List.tl aux) in
        let values = String.split_on_char ',' values in
        let values = List.map String.trim values in
        add_field field_map field values
    ) FieldMap.empty field_lines
  in

  { typ = method_ty; uri = request_uri;
    fields = field_map; content = content }

let create inet_addr =
  let tcp_protocol = 6 in
  Logger.info (fun m -> m "Starting server on %a" print_address inet_addr);
  let sock = socket PF_INET SOCK_STREAM tcp_protocol in
  setsockopt sock SO_REUSEADDR true;
  bind sock inet_addr;
  { addr = inet_addr; sock = sock }

let close serv =
  shutdown (sock serv) SHUTDOWN_ALL;
  Unix.close (sock serv);
  Logger.info (fun m -> m "Server closed")

let listen_and_accept serv =
  listen (sock serv) 1;
  Logger.info (fun m -> m "%s" "Listening to new connections");
  let client_sock, client_addr = accept (sock serv) in
  Logger.info (fun m -> m "Got a connection from %a" print_address client_addr);
  client_sock, client_addr

let send_response sock status fields =
  let message = "HTTP/1.1 " ^ status ^ "\n" ^
                (String.concat "\n"
                   (List.map (fun (field,value) -> field ^ ":" ^ value)
                      fields)) ^
                "\n\n"
  in
  send sock (Bytes.of_string message) 0 (String.length message) [];
  Logger.info (fun m -> m "Sent:\n%s" message)


let check_ws_opening_request req =
  (* check requirements of the opening request, RFC page 21 *)

  let check_key key =
    if not (FieldMap.mem key req.fields) then
      raise (WSError ("The opening HTTP request MUST contain a \"" ^ key ^
                      "\" field"))
  in
  let check_val key value =
    check_key key;
    let field = FieldMap.find key req.fields in
    if List.length field = 1 then
      if String.lowercase_ascii (List.hd field) = "websocket" then ()
    else
      raise (WSError ("The opening HTTP request MUST contain an \"" ^ key ^
                      "\" field with value \"" ^ value ^ "\""))
  in
  let include_val key value =
    check_key key;
    let lowercase_value = String.lowercase_ascii value in
    let values = FieldMap.find key req.fields in
    let lowercase_values = List.map String.lowercase_ascii values in
    if not (List.mem lowercase_value lowercase_values) then
      raise (WSError ("The opening HTTP request MUST contain a \"" ^ key ^
                      "\" field including the value \"" ^ value ^ "\""))
  in


  (* 1 - Request has to be a GET request*)
  if req.typ <> GET then
    raise (WSError ("The opening HTTP request MUST be a GET, not a " ^
                    (string_of_method_ty req.typ)));

  (* 2 - A |Host| header field *)
  check_key "Host";

  (* 3 - A |Upgrade| header field containing the value "websocket" *)
  check_val "Upgrade" "websocket";

  (* 4 - A |Connection| header field that includes the token "Upgrade" *)
  include_val "Connection" "Upgrade";

  (* 5 - A |Sec-WebSocket-Key| header field with base64-encoded value that,
         when decoded, is 16 bytes in length *)
  let encoded_ws_key = FieldMap.find "Sec-WebSocket-Key" req.fields in
  if List.length encoded_ws_key <> 1 then
    raise (WSError ("The field Sec-WebSocket-Key MUST have one (and only one) value"));

  let decoded_ws_key = Base64.decode (List.hd encoded_ws_key) in
  if String.length decoded_ws_key <> 16 then
    raise (WSError ("The decoded WebSocket key has length " ^
                    (string_of_int (String.length decoded_ws_key)) ^
                    " while it was expected to have length 16"));

  (* 6 - A |Sec-WebSocket-Version| header field *)
  check_key "Sec-WebSocket-Version";

  (* 7 - Optionnally, an |Origin| field *)
  (* 8 - Optionnally, a |Sec-WebSocket-Protocol| field *)
  (* 9 - Optionnally, a |Sec-WebSocket-Extensions| field *)
  (* 10 - Optionnally, other fields *)
  ()

let do_ws_handshake sock =
  let rcv_buffer = Bytes.create 1024 in
  recv sock rcv_buffer 0 1024 [];
  let req = Bytes.to_string rcv_buffer in
  (* If the message is more than 1024 bytes long, this won't work *)
  let req = String.sub req 0 (String.index req '\000') in
  Logger.info (fun m -> m "Received message:\n%s\n" req);
  let req = parse_request req in

  (* NOTE : req.uri is a /ressource name/

     cited from page 14 of RFC6455 :

    The ressource name can be constructed by concatenating the following :
     - "/" if the path component is empty
     - the path component
     - "?" if the query component is non-empty
     - the query component

     END OF CITATION

     ie the uri looks like ("/" | PATH)[?(QUERY)*]
  *)

  check_ws_opening_request req;

  (* build response to opening request, page 22 *)

  (* get origin key if present *)
  let origin =
    match FieldMap.find_opt "Origin" req.fields with
    | None -> None
    | Some origin ->
      if List.length origin <> 1 then
        raise (WSError ("The optional field Origin MUST have one and only one value"));
      let origin = String.lowercase_ascii (List.hd origin) in
      Some origin
  in

  (* get ws key *)
  (* size of list has been checked before *)
  let encoded_ws_key = List.hd (FieldMap.find "Sec-WebSocket-Key" req.fields) in

  (* get version *)
  let version = FieldMap.find "Sec-WebSocket-Version" req.fields in
  if List.length version <> 1 then
    raise (WSError ("The field Sec-WebSocket-Version MUST have one and only one value"));
  if (List.hd version) <> "13" then begin
    send_response sock "426 Upgrade Required" [
      "Sec-WebSocket-Version", "13" ];
    raise (WSError ("Unsupported WebSocket version" ^ (List.hd version)))
  end;

  (* subprotocol to use : none implemented yet *)
  (* extensions to use : none implemented yet *)

  send_response sock "101 Switching Protocols" [
    "Upgrade", "websocket";
    "Connection", "Upgrade";
    "Sec-WebSocket-Accept", build_server_key encoded_ws_key;
  ]

let local_http () =
  let address = inet_addr_of_string "127.0.0.1" in
  let port = 8080 in
  let inet_addr = ADDR_INET(address, port) in
  let connection_server = create inet_addr in
  let client_sock, client_addr = listen_and_accept connection_server in
  begin
    try
      do_ws_handshake client_sock;
      let websock = Websocket.assign client_sock client_addr in
      let Some msg = Websocket.receive_message websock in
      Logger.info (fun m -> m "Received message: %s"
                      begin match msg.msg_typ with
                        | BinaryMsg -> "binary of length " ^ (string_of_int (Bytes.length msg.msg_data))
                        | TextMsg -> Bytes.to_string msg.msg_data
                      end);
      Websocket.send_text websock "YOU rock bro!";
      Logger.info (fun m -> m "Sent message : %s" "YOU rock bro!");
      Websocket.send_ping websock (Bytes.of_string "Hello world");
      Logger.info (fun m -> m "Sent ping");
      let msg = Websocket.receive_message websock in
      Logger.info (fun m -> m "Read message");
      begin match msg with
      | None -> Logger.info (fun m -> m "empty message")
      | Some msg -> Logger.info (fun m -> m "Got : %s" (Bytes.to_string msg.msg_data))
      end;
      ()
    with e -> Logger.error (fun m -> m "Exception : %s" (Printexc.to_string e))
  end;
  close connection_server
