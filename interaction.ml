open Netlist_ast
       
let rec bin_of_intasbin v = 
  let d, r = v/2, v mod 2 in
  if d = 0 then
    [r = 1]
  else
    (bin_of_intasbin d) @ [r = 1]

let stringint_of_bool b = 
  if b then "1"
  else "0"

let int_of_bool b =
  if b then 1
  else 0

let bool_of_int i =
  let rec aux n =
    let b =
      match n mod 10 with
      | 0 -> false
      | 1 -> true
      | i -> Format.eprintf "Unexpected: %d@." i; raise Parsing.Parse_error
    in
    if n < 10 then
      [b]
    else
      b::(aux (n / 10))
  in
  match aux i with
  | [] -> Format.eprintf "Empty list@."; raise Parsing.Parse_error
  | b -> Array.of_list b
			  
let boolarray_of_string str size =
  let i = int_of_string str in
  let arr = bool_of_int i in
  let l = Array.length arr in
  if l = size then
    arr
  else if l < size then (
    Array.append (Array.make (size-l) false) arr)
  else(
    Array.sub arr 0 size )
		   
let int_of_boolarray bool_ar =
  Array.fold_right (fun value computed ->
		    (int_of_bool value) + 2*computed )  bool_ar 0

let int_of_val value = match value with
  | VBit b -> int_of_bool b
  | VBitArray bar -> int_of_boolarray bar

let rec string_of_value value = match value with
  | VBit b -> stringint_of_bool b
  | VBitArray ar -> 
     Array.fold_left (fun string bool -> string ^ (stringint_of_bool bool)) "" ar

let ask_value s n =
  let message = (
    if n = 1 then
      ""
    else
      "["^(string_of_int (n))^"]"
  ) in
  
  let rec inner s = 
    if n = 0 then Array.make 0 false
    else (
      print_string (s^(message)^"? ");
      let v = read_line () in
      try
	if String.length v != n then (
	  raise (Failure ("String too short"))
	)
	else
	  boolarray_of_string v n
      with Failure "String too short" | Failure "int_of_string"-> 
      	print_string "Invalid value, O/1 list expected. ";
      	inner s
	   | _ -> raise (Failure "Unknown error!")

    )
  in
  let list = inner s in
  if (Array.length list > 1) then
    VBitArray list
  else
    VBit list.(0)

let print_value v = match v with
  | VBit b -> stringint_of_bool b
  | VBitArray ar ->
     Array.fold_left (fun s v -> s^(stringint_of_bool v)) "" ar
		     
let dump table =
  Hashtbl.iter (fun key value -> print_endline (key^":\t"^(print_value value))) table 
	       
let check_set r v =
  if !r = 0 then
    r := v
  else
    assert (!r = v)
