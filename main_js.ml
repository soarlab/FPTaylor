open Js_of_ocaml

(* let append_string output cl s =
  let open Js_of_ocaml in
  let d = Dom_html.window##.document in
  let span = Dom_html.createDiv d in
  span##.classList##add (Js.string cl);
  Dom.appendChild span (d##createTextNode (Js.string s));
  Dom.appendChild output span

let init_out chan =
  let out = Dom_html.getElementById "output" in
  let flush s =
    (* let text = Js.to_string out##.innerHTML in *)
    (* out##.innerHTML := Js.string (text ^ s) in *)
    append_string out "test" s in
  Sys_js.set_channel_flusher chan flush *)

(* let get_input_textarea () =
  match Dom_html.getElementById_coerce "input" Dom_html.CoerceTo.textarea with
  | None -> failwith "No input textarea"
  | Some v -> v *)

let init_out chan =
  let flush s = Worker.post_message (Js.string s) in
  Sys_js.set_channel_flusher chan flush

let init_fs () =
  Sys_js.create_file "./default.cfg" Default.default_cfg;
  Sys_js.create_file "user.cfg" "verbosity = 1";
  Sys_js.create_file "input.txt" "";
  ()

let run_fptaylor () =
  Config.init ["user.cfg"];
  Fptaylor.fptaylor ["input.txt"]

(* let init_button () =
  let input = get_input_textarea () in
  let counter = ref 0 in
  let button = Dom_html.getElementById "button" in
  button##.onclick := Dom_html.handler (fun _ -> 
    incr counter;
    Printf.printf "Button clicked: %d\n" !counter;
    Sys_js.update_file "input.txt" (Js.to_string input##.value);
    run_fptaylor ();
    Js._true) *)

(* let init () =
  Log.report `Main "FPTaylor, version %s" Version.version;
  init_out Stdlib.stdout;
  init_out Stdlib.stderr;
  init_fs ();
  init_button () *)

(* let () = Dom_html.window##.onload := 
  Dom_html.handler (fun _ ->
    init ();
    Js._false) *)

let () =
  init_out Stdlib.stdout;
  init_out Stdlib.stderr;
  init_fs ()

let () =
  Worker.set_onmessage (fun msg ->
    Sys_js.update_file "input.txt" (Js.to_string msg);
    run_fptaylor ())
