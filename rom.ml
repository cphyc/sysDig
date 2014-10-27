let read_file filename =
  let find_file filename =
    try
      open_in filename
    with
    | _ -> raise (Failure ("No such file "^filename))
  in
  let ic = find_file filename in
  let buf = Lexing.from_channel ic in
  try
    Rom_lexer.token buf;
  with
  | Rom_lexer.Eof -> ()
  | Rom_lexer.Lexing_error _ ->
     raise (Failure "Erreur dans l'analyse lexicale")
	       
let create filename =
  let rom_name = (Filename.chop_suffix filename ".net") ^ ".rom" in
  read_file rom_name ;
  print_endline ("Created ROM of size "^(string_of_int !Rom_comm.addr_size) ^
		   "*" ^ (string_of_int !Rom_comm.word_size));
  Array.iteri (fun i v ->  print_string (Interaction.stringint_of_bool v)) !Rom_comm.rom
