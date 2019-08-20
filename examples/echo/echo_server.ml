let msgs = [
  "YOU are breathtaking!";
  "YOU are breathtaking!";
  "YOU are breathtaking!";
  "YOU are breathtaking!";
  "YOU are breathtaking!";
  "YOU are breathtaking!";
  "It was fun, let's stop playing not";
  "We should both go back to work";
  "Please, stop this";
  "I'm asking you to stop pushing that button";
  "Are you listening to me ?";
  "Stop it already";
  "I'm not answering you anymore";
  "I mean it";
  "Hit the DISCONNECT button";
  "I will disconnect if you continue";
  "Ok stop please";
  "I said stop";
  "Seriously ?";
  "I'M OUT !!";
]

let _ =
  (* server on localhost *)
  let address = Unix.inet_addr_of_string "127.0.0.1" in
  let port = 8080 in
  let inet_addr = Unix.ADDR_INET(address, port) in

  (* create the small server *)
  let connection_server = Websocketml.http_create inet_addr in

  (* wait for a connexion *)
  let client_sock, client_addr = Websocketml.http_listen_and_accept connection_server in

  begin
    try
      (* do the websocket handshake,
         if there is no exception, everything is ok *)
      Websocketml.http_do_ws_handshake client_sock;

      (* create the websocket connection *)
      let websock = Websocketml.create client_sock client_addr in

      (* listen for a websocket message *)
      (* Note : it can be None, for example if the first frame
         is a CLOSE frame *)
      let msg = Websocketml.receive_message websock in
      begin match msg with
        | None -> ()
        | Some msg ->
          (* print the message *)
          Logger.info (fun m -> m "Received message: %s"
                          begin match msg.msg_typ with
                            | BinaryMsg -> "binary of length " ^ (string_of_int (Bytes.length msg.msg_data))
                            | TextMsg -> Bytes.to_string msg.msg_data
                          end);
      end;

      (* send something back *)
      let to_send = "YOU are breathtaking!" in
      ignore (Websocketml.send_text websock to_send);
      Logger.info (fun m -> m "Sent message : %s" to_send);

      (* send a ping message*)
      ignore (Websocketml.send_ping websock (Bytes.of_string "Hello world"));
      Logger.info (fun m -> m "Sent ping");

      (* this time we expect to get a CLOSE message, msg can be None *)
      Logger.info (fun m -> m "Waiting for message");
      let msg = ref (Websocketml.receive_message websock) in
      (* the stack of answers *)
      let msg_stack = ref msgs in

      while not (Websocketml.closed websock) && (List.length !msg_stack > 0) do

        begin match !msg with
          | None -> Logger.info (fun m -> m "empty message")
          | Some msg -> Logger.info (fun m -> m "Got : %s" (Bytes.to_string msg.msg_data))
        end;

        (* send one of the answers *)
        let to_send = (List.hd !msg_stack) in
        ignore (Websocketml.send_text websock to_send);
        Logger.info (fun m -> m "Sent message : %s" to_send);
        msg_stack := List.tl !msg_stack;

        (* if there are more answers we continue listening, else we disconnect *)
        if List.length !msg_stack > 0 then begin
          Logger.info (fun m -> m "Waiting for message");
          msg := Websocketml.receive_message websock
        end else
          ignore (Websocketml.close websock Websocketml.NormalClosure);
      done

    with e -> Logger.error (fun m -> m "Exception : %s" (Printexc.to_string e))
  end;
  Websocketml.http_close connection_server
