open Netlist_ast
let addr_size = ref (0)
let word_size = ref (0)
let ram = ref (Array.make_matrix 0 0 ( ref (0) ))
	      
let create p =
  (* find the occurences of ERam and check consistency *)
  let iter eq = match eq with 
	   | _, (Eram (address_size_, word_size_, _, _, _, _)) ->
	      Interaction.check_set addr_size address_size_;
	      Interaction.check_set word_size word_size_;
	   | _, _ -> ()
  in
  List.iter iter p.p_eqs;
  print_endline
    ("Creating RAM of size "^(string_of_int (1 lsl !addr_size))^"*"^(string_of_int !word_size));
  ram := Array.make_matrix (1 lsl !addr_size) !word_size (ref 0)

