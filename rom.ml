let rom = ref (Array.make_matrix 0 0 false)
			    
let addr_size = ref (0)
let word_size = ref (0)

let create () =
  rom := Array.make_matrix (1 lsl !addr_size) !word_size false
			   
let load address value =
  !rom.address <- value

let read_file filename =
  let find_file filename =
    try
      open_in filename
    with
    | _ -> raise (Parse_error "No such file '%s'")
  in
  let ic = find_file filename in
  let buf = Lexing.from_channel ic in
  try
    Rom_lexer.token buf
  with
  | Rom_lexer.Lexing_error _ ->
     raise (Failure "Erreur dans l'analyse lexicale")
	       
let create filename =
  let rom_name = (Filename.chop_suffix filename ".net") ^ ".rom" in
  read_file rom_name;
  dump
  
