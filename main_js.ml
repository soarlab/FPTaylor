open Js_of_ocaml

let init_out ty chan =
  let flush s = 
    let obj = object%js
      val ty = ty
      val str = Js.string s
    end in
    Worker.post_message obj in
  Sys_js.set_channel_flusher chan flush

let init_fs () =
  Sys_js.create_file "./default.cfg" Default.default_cfg;
  Sys_js.create_file "user.cfg" "verbosity = 1";
  Sys_js.create_file "input.txt" "";
  ()

let run_fptaylor () =
  Config.init ["user.cfg"];
  Fptaylor.fptaylor ["input.txt"]

let () =
  init_out 1 Stdlib.stdout;
  init_out 2 Stdlib.stderr;
  init_fs ()

class type js_msg_type = object
  method input : Js.js_string Js.t Js.readonly_prop
  method config : Js.js_string Js.t Js.readonly_prop
end

let () =
  Worker.set_onmessage (fun (msg : js_msg_type Js.t) ->
    let input = msg##.input |> Js.to_string in
    let config = msg##.config |> Js.to_string in
    Sys_js.update_file "input.txt" input;
    Sys_js.update_file "user.cfg" config;
    run_fptaylor ())
