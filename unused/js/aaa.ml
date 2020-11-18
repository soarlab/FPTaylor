let load_and_close_channel do_close ic = 
  let rec lf ichan a = 
    try
      lf ic (Stdlib.input_line ic :: a)
    with End_of_file -> a in
  let rs = lf ic [] in
  if do_close then Stdlib.close_in ic;
  List.rev rs

let load_file filename = 
  let ic = Stdlib.open_in filename in
  load_and_close_channel true ic

open Js_of_ocaml

let append_string output cl s =
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
  Sys_js.set_channel_flusher chan flush

let init_fs () =
  let input = 
    match Dom_html.getElementById_coerce "input" Dom_html.CoerceTo.textarea with
    | None -> failwith "No input textarea"
    | Some v -> v in
  Sys_js.mount_point ()
    |> List.iter (Printf.printf "%s\n");
  Sys_js.create_file "test.cfg" "Test configuration";
  Sys_js.create_file "input.txt" "Empty input";
  (* Callback's value is cached so it is not very useful *)
  Sys_js.mount "files" (fun ~prefix ~path ->
    Printf.printf "prefix = %s, path = %s\n" prefix path;
    let text = Js.to_string input##.value in
    Some text)

let init_button () =
  let input = 
    match Dom_html.getElementById_coerce "input" Dom_html.CoerceTo.textarea with
    | None -> failwith "No input textarea"
    | Some v -> v in
  let counter = ref 0 in
  let button = Dom_html.getElementById "button" in
  button##.onclick := Dom_html.handler (fun _ -> 
    incr counter;
    Printf.printf "Button clicked: %d\n" !counter;
    Sys_js.update_file "input.txt" (Js.to_string input##.value);
    let lines = load_file "input.txt" in
    List.iter (Printf.printf "%s\n") lines;
    Js._true)

let init () =
  try
    init_out Stdlib.stdout;
    init_fs ();
    init_button ();
    (* let lines = load_file "files/default.cfg" in *)
    let lines = load_file "test.cfg" @ load_file "files/default.cfg" in
    (* let lines = ["Test"; "lines"] in *)
    List.iter (Printf.printf "%s\n") lines
  with Not_found ->
    Printf.printf "Error: Not_found"

let () = Dom_html.window##.onload := 
  Dom_html.handler (fun _ ->
    init();
    Js._false)