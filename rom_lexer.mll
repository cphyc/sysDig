{
  open Lexing
  open Rom_comm
  exception Lexing_error of string
  exception Eof
	      
  let make_rom () =
    rom := Array.make (1 lsl !addr_size) (Array.make 1 false)
			   
  let save address value =
    (* Copy value from 0 to !word_size into !rom at position address *)
    !rom.(address) <- value

 let value_of_int n =
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
   match aux n with
     | [] -> Format.eprintf "Empty list@."; raise Parsing.Parse_error
     | b -> Array.of_list b
}

let space = [' ' '\t']
let eol = ['\n']
let bin_digit = ['0' '1']
let bin = bin_digit+
  
rule token = parse
| "WORD SIZE" space+ (bin as ws_lxm) eol "ADDRESS SIZE" space+ (bin as as_lxm) eol
| "ADDRESS SIZE" space+ (bin as as_lxm) eol "WORD SIZE" space+ (bin as ws_lxm) 
  { word_size := int_of_string ("0b" ^ ws_lxm) ; 
    addr_size := int_of_string ("0b" ^ as_lxm) ;
    make_rom ();
    token lexbuf }
| (bin as address) space+ (bin as value)
  { (* Get the address as an int *)
    let address_as_int = int_of_string ("0b"^address) in
    (* Get the value as an array of bool *)
    let value_as_bool_array = Interaction.boolarray_of_string value !word_size in
    (* Add it *)
    save (address_as_int) (value_as_bool_array);

    token lexbuf
  }
| eol { Lexing.new_line lexbuf; token lexbuf }
| eof { raise Eof }
| _ as c { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }
