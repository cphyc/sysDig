let read_file filename =
  try
    let ic = open_in filename in
    let buf = Lexing.from_channel ic in
    Rom_lexer.token buf;
    close_in ic;
  with
  | Sys_error _ -> ()
  | Rom_lexer.Eof -> ()
  | Rom_lexer.Lexing_error _ ->
     raise (Failure "Lexical analysis error.")
	       
let create filename =
  let rom_name = (Filename.chop_suffix filename ".net") ^ ".rom" in
  read_file rom_name ;

  (* Array.iter (fun ar -> Array.iter *)
  (* 			  (fun v -> *)
  (* 			   print_string (Interaction.stringint_of_bool v) *)
  (* 			  ) ar *)
  (* 	     ) *)
  (* 	     !Rom_comm.rom *)
  (* () *)
