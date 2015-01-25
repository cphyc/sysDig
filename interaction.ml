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

let stringint_of_boolarray barray = 
  let rec aux arg = match arg with
    | [] -> ""
    | b :: bl -> (stringint_of_bool b) ^ aux bl
  in
  aux (Array.to_list barray)

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
      (aux (n / 10)) @ [b]
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
  else if l < size then
    Array.append (Array.make (size-l) false) arr
  else
    Array.sub arr 0 size

let rec boolarray_of_int int size =
  let rec aux i = 
    let high_byte = i lsr 1 in
    let low_byte = (i mod 2) == 1 in
    
    if high_byte > 0 then
      (aux high_byte) @ [low_byte]
    else
      [low_byte]
  in
  let barray = aux int in
  let rec aux2 barray =
    if List.length barray == size then
      barray
    else
      aux2 ([false] @ barray)
  in
  Array.of_list (aux2 barray)
   
  
let int_of_boolarray bool_ar =
  Array.fold_left (fun computed value ->
		   (int_of_bool value) + 2*computed ) 0 bool_ar

let int_of_val value = match value with
  | VBit b -> int_of_bool b
  | VBitArray bar -> int_of_boolarray bar

let rec string_of_value value = match value with
  | VBit b -> stringint_of_bool b
  | VBitArray ar -> 
     Array.fold_left (fun string bool -> string ^ (stringint_of_bool bool)) "" ar

let rec bin_of_int i =
  let high_byte = i lsr 1 in
  let low_byte = ( i mod 2) in

  if high_byte > 0 then
    low_byte+10*(bin_of_int high_byte)
  else
    low_byte
      
  
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
