let print_only = ref false
let number_steps = ref (-1)

let compile filename =
  try
    let p_unscheduled = Netlist.read_file filename in
    let out_name = (Filename.chop_suffix filename ".net") ^ "_sch.net" in
    let p =
      (if not (Sys.file_exists out_name) then(
	 print_endline "Scheduling";
	 let out = open_out out_name in
	     
	 (* Schedule the program *)
	 let p = Scheduler.schedule p_unscheduled  in
	 (* Write the output to the _sch.net file*)
	 Netlist_printer.print_program out p;
	 p
       )
       else
	 p_unscheduled
      )
    in
    
    if  !print_only then
      (* Also print it to stdout *)
      Netlist_printer.print_program stdout p
    else
      (* Create the rom *)
      Rom.create filename;

      (* Execute it *)
      Simulator.execute p !number_steps
      
  with
  | Scheduler.Combinational_cycle ->
     Format.eprintf "The netlist has a combinatory cycle.@.";
     exit 2
  | Netlist.Parse_error s ->
     Format.eprintf "An error occurred: %s@." s;
     exit 2
  (* | _ -> *)
  (*    Format.eprintf "Unknown error"; *)
  (*    exit 2 *)


let main () =
  Arg.parse
    ["-v", Arg.Set print_only, "Only print the result of scheduling";
     "-n", Arg.Set_int number_steps, "Number of steps to simulate"]
    compile
    ""
;;

let () = if not !Sys.interactive then main ()
