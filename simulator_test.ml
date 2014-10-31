let print_only = ref false
let number_steps = ref (-1)

let compile filename =
  try
    let p = Netlist.read_file filename in
    let out_name = (Filename.chop_suffix filename ".net") ^ "_sch.net" in
    let out = open_out out_name in
    let close_all () =
      close_out out
    in
    let p = try
        Scheduler.schedule p 
      with
      | Scheduler.Combinational_cycle ->
         Format.eprintf "The netlist has a combinatory cycle.@.";
         close_all ();
	 exit 2
      | Netlist.Parse_error s ->
	 Format.eprintf "An error occurred: %s@." s;
	 close_all ();
	 exit 2
    in
    close_all ();
    
    (
      if not !print_only then (
	let p_scheduled = Scheduler.schedule p in
	(* Create the rom *)
	Rom.create filename;
	(* Execute the program *)
	Simulator.execute p_scheduled !number_steps;
      )
      else (
	Netlist_printer.print_program stdout p;
      )
    )
    
    
let main () =
  Arg.parse
    ["-v", Arg.Set print_only, "Only print the result of scheduling";
     "-n", Arg.Set_int number_steps, "Number of steps to simulate"]
    compile
    ""
;;

let () = if not !Sys.interactive then main ()
