open Netlist_ast

let rec bin_of_int v = 
  let d, r = v/2, v mod 2 in
  if d = 0 then
    [r = 1]
  else
    (bin_of_int d) @[r = 1]

let stringint_of_bool b = 
  if b then "1"
  else "0"

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
	let input =
	  if v = "1" then true
	  else if v = "0" then false
	  else (bool_of_string v)
	in
	input :: (inner s (n-1))
      with _ -> 
	print_string "Invalid value, true/false expected. ";
	inner s n 
    )
  in
  let list = inner s nmax in
  if (List.length list > 1) then
    VBitArray (Array.of_list list)
  else
    VBit (List.hd list)

