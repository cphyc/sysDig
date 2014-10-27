{
  open Ram
  open Lexing
  exception Lexing_error of string
}

let space = [' ' '\t']
let eol = ['\n']
let bin_digit = ['0' '1']
let bin = bin_digit+
  
rule token = parse
| "WORD SIZE" space+ (bin as lxm) eol "ADDRESS SIZE" space+ (bin as lxm) eol
  { word_size := int_of_string lxm ;
    addr_size := int_of_string lxm ;
    token lexbuf }
| (bin as address) space+ (bin as value) eol
      { load_to_ram }
| eof { EOF }
| _ as c { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }
