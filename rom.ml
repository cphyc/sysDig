let read_file filename =
  try
    let ic = open_in filename in
    let buf = Lexing.from_channel ic in
    Rom_lexer.token buf;
  with
  | Sys_error _ -> ()
  | Rom_lexer.Eof -> ()
  | Rom_lexer.Lexing_error _ ->
     raise (Failure "Lexical analysis error.")
	       
let create filename =
  let rom_name = (Filename.chop_suffix filename ".net") ^ ".rom" in
  read_file rom_name ;
  print_endline ("Created ROM of size "^
		   (string_of_int (1 lsl !Rom_comm.addr_size)));
  Array.iter (fun ar -> Array.iter
			  (fun v ->
			   print_string (Interaction.stringint_of_bool v)
			  ) ar
	     )
	     !Rom_comm.rom
