let main () =
  Config.init ();
  Fptaylor.fptaylor ()

let () = main ()