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
    begin try
        let p = Scheduler.schedule p in
	print_endline "Simulation the following program:";
        Netlist_printer.print_program stdout p;
	print_endline "#################################";
      with
        | Scheduler.Combinational_cycle ->
            Format.eprintf "The netlist has a combinatory cycle.@.";
            close_all (); exit 2
    end;
    close_all ();
    if not !print_only then (
      let n = if !number_steps = -1 then 1 else !number_steps in
      
      let p_scheduled = Scheduler.schedule p in
      let output = Simulator.execute p_scheduled n in
      List.iter (fun (ident, value) -> 
		 print_endline
		   (ident^" = "^(Interaction.string_of_value value))
		) output
    )
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2

let main () =
  Arg.parse
    ["-print", Arg.Set print_only, "Only print the result of scheduling";
     "-n", Arg.Set_int number_steps, "Number of steps to simulate"]
    compile
    ""
;;

main ()