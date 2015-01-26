open Netlist_ast
       
let addr_size = ref (0)
let word_size = ref (0)
		    
let ram = ref (Array.make_matrix 0 0 false)
let ram_stack = Stack.create ()
		    
let create p =
  (* find the occurences of ERam and check consistency *)
  let iter eq = match eq with 
	   | _, (Eram (address_size_, word_size_, _, _, _, _)) ->
	      Interaction.check_set addr_size address_size_;
	      Interaction.check_set word_size word_size_;
	   | _, _ -> ()
  in
  List.iter iter p.p_eqs;
  let addr_array_size =
    if 1 lsl !addr_size < 10 then
      16
    else
      1 lsl !addr_size
  in
  let word_array_size =
    if !word_size == 0 then
      8
    else
      !word_size
  in
  ram := Array.make_matrix addr_array_size word_array_size false

let push write_addr data =
  Stack.push (write_addr, data) ram_stack
				       
let process_queue () =
  let rev array = Array.of_list (List.rev (Array.to_list array)) in
  Stack.iter (fun (write_addr, data) ->
	      Printf.printf "Wrote %s at %d\n" (Interaction.stringint_of_boolarray data) write_addr;
	      !ram.(write_addr) <- data) ram_stack;
  Stack.clear ram_stack
	    
