open Netlist_ast
       
let rec bin_of_int v = 
  let d, r = v/2, v mod 2 in
  if d = 0 then
    [r = 1]
  else
    (bin_of_int d) @ [r = 1]

let stringint_of_bool b = 
  if b then "1"
  else "0"

let int_of_bool b =
  if b then 1
  else 0

let boolarray_of_string str =
  let i = int_of_string ("0b" ^ str) in
  bin_of_int i
		   
let int_of_boolarray bool_ar =
  Array.fold_right (fun value computed -> (int_of_bool value) + 2*computed )  bool_ar 0

let int_of_val value = match value with
  | VBit b -> int_of_bool b
  | VBitArray bar -> int_of_boolarray bar

let rec string_of_value value = match value with
  | VBit b -> stringint_of_bool b
  | VBitArray ar -> 
     Array.fold_left (fun string bool -> string ^ (stringint_of_bool bool)) "" ar

let ask_value s nmax =
  let message n = (
    if nmax = 1 then
      ""
    else
      "["^(string_of_int (nmax - n))^"]"
  ) in
  
  let rec inner s n = 
    if n = 0 then []
    else (
      print_string (s^(message n)^"? ");
      
      let v = read_line () in
      try  
	boolarray_of_string v
      with _ -> 
	print_string "Invalid value, O/1 list expected. ";
	inner s n 
    )
  in
  let list = inner s nmax in
  if (List.length list > 1) then
    VBitArray (Array.of_list list)
  else
    VBit (List.hd list)

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
