let rom = ref (Array.make_matrix 0 0 false)
			    
let addr_size = ref (0)
let word_size = ref (0)
		    
let load_to_ram address value =
  assert false

let find_file filename =
  try
    open_in filename
  with
    | _ -> raise (Parse_error "No such file '%s'")

let read_file filename =
  let ic = find_file filename in
  let buf = Lexing.from_channel ic in
  try
    Ram_lexer.token buf
  with
  | Ram_lexer.Lexing_error _ ->
     raise (Failure "Erreur dans l'analyse lexicale")
	       
let create_rom filename =
  let rom_name = (Filename.chop_suffix filename ".net") ^ ".rom" in
  let rom = open_in rom_name in

  let lexbuf = Lexing.from_channel 
