module EscCodes = struct
  let none = "\027[0;0m"

  let bold  = "\027[0;1m"
  let faint = "\027[0;2m"

  let italic      = "\027[0;3m"
  let underline   = "\027[0;4m"
  let slow_blink  = "\027[0;5m"
  let rapid_blink = "\027[0;6m"

  let black        = "\027[0;30m"
  let dark_gray    = "\027[1;30m"
  let red          = "\027[0;31m"
  let light_red    = "\027[1;31m"
  let green        = "\027[0;32m"
  let light_green  = "\027[1;32m"
  let orange       = "\027[0;33m"
  let yellow       = "\027[1;33m"
  let blue         = "\027[0;34m"
  let light_blue   = "\027[1;34m"
  let purple       = "\027[0;35m"
  let light_purple = "\027[1;35m"
  let cyan         = "\027[0;36m"
  let light_cyan   = "\027[1;36m"
  let light_gray   = "\027[0;37m"
  let white        = "\027[1;37m"
end

let verbose = ref 1

let print ff color prefix f =
  Printf.fprintf ff "%s%-7s " color ("[" ^ prefix ^ "]");
  f (Printf.fprintf ff);
  Printf.fprintf ff "%s\n" EscCodes.none;
  flush stdout

let print_level level ff color prefix f =
  if level <= !verbose then print ff color prefix f

let error f = print_level 0 stderr EscCodes.red "ERROR" f
let warn f = print_level 1 stderr EscCodes.orange "WARN" f

let info f = print_level 0 stdout EscCodes.light_blue "INFO" f
let debug f = print_level 1 stdout EscCodes.yellow "DEBUG" f

(* Test *)
(* let _ =
  error (fun m -> m "%s" "ERROR !!");
  info (fun m -> m "%s" "info");
  warn (fun m -> m "%s" "Warn");
  debug (fun m -> m "%s" "debug");
  () *)
