open Interaction
open Netlist_ast
open Scheduler 

let env = Hashtbl.create 73;;
let reg = Hashtbl.create 73;;
  
let read_arg arg = match arg with
  | Avar i -> Hashtbl.find env i
  | Aconst vbit -> vbit

let binop op a b = match op with 
  | Or -> a || b
  | And -> a && b
  | Xor -> (a && not(b)) || (not(a) && b)
  | Nand -> not(a && b)


let ex_eq eq = 
  let ident, expression = eq in
  let ex_exp exp = 
    match exp with
    | Earg arg -> read_arg arg
    | Ereg i ->  (* do reg[ident] <- env[i] *)
       Hashtbl.replace reg ident (Hashtbl.find env i);
       (* return the current value of the ident *)
       Hashtbl.find env ident
    | Enot arg -> 
       begin 
	 match read_arg arg with
	 | VBit b -> VBit (not(b))
	 | VBitArray ar -> VBitArray (Array.map not ar)
       end
    | Ebinop (op, arg1, arg2) ->
       begin
	 let v1, v2 = (read_arg arg1), (read_arg arg2) in
	 match v1, v2 with
	 | VBit b1, VBit b2 -> VBit (binop op b1 b2)
	 | VBitArray ar1, VBitArray ar2 
	      when (Array.length ar1) = (Array.length ar2) ->
	    VBitArray (Array.mapi (fun i _ -> binop op ar1.(i) ar2.(i)) ar1)
	 | _ -> raise (Failure ("Incompatible types 1"))
       end				  
    | Emux (s, a, b) -> 
       begin
	 let mux a b c =
	   (a && b) || (not(a) && c)
	 in
	 let vs = read_arg s in
	 let va = read_arg a in
	 let vb = read_arg b in
	 match vs, va, vb with
	 | VBit b1, VBit b2, VBit b3 -> VBit (mux b1 b2 b3)
	 | VBitArray ar1, VBitArray ar2, VBitArray ar3 -> 
	    VBitArray (Array.mapi (fun i ar1i -> mux ar1i ar2.(i) ar3.(i)) ar1)
	 | _ -> raise (Failure ("Incompatible types 2")) 
       end
    | Erom (addr_size, word_size, read_addr) -> assert false
    | Eram (addr_size, word_size, read_addr, write_enable, write_addr, data ) -> assert false
    | Econcat (arg1, arg2) ->
       begin
	 let v1, v2 = (read_arg arg1), (read_arg arg2) in
	 match v1, v2 with
	 | VBit b1, VBit b2 -> VBitArray (Array.of_list (b1 :: [b2]))
	 | VBitArray ar1, VBitArray ar2 -> VBitArray (Array.append ar1 ar2)
	 | _ -> raise (Failure ("Incompatible types 3"))
       end
    | Eslice (int1, int2, arg) -> assert false
    | Eselect (int, arg) -> assert false
  in
  let value = ex_exp expression in
  (* TODO: optimize here to avoir setting ident to its previous value in case
     of a register *)
  Hashtbl.replace env ident value

let execute p n =
  (* Add the variables *)
  Env.iter (fun ident tbit -> 
	    let vbit = match tbit with
	      | TBit -> VBit false
	      | TBitArray l -> VBitArray (Array.make l false)
	    in Hashtbl.add env ident vbit ) p.p_vars;

  (* Fill the input variables *)
  List.iter ( fun ident ->
	      let n = match (Env.find ident p.p_vars) with 
		| TBit -> 1 | TBitArray n -> n 
	      in
	      let v = ask_value ident n in
	      Hashtbl.replace env ident v
	    ) p.p_inputs ;

  (* Executes the program *)
  let _ = 
    for i = 1 to n do
      print_endline (string_of_int i);
      List.iter ex_eq p.p_eqs;
      (* Move the reg into the env *)
      Hashtbl.iter (fun key value -> Hashtbl.replace env key value) reg;
    done
  in

  (* Returns the outputs *)
  List.fold_left (fun list output_ident -> 
		  (output_ident, Hashtbl.find env output_ident) :: list)
		 [] p.p_outputs
