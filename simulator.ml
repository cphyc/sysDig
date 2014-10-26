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
       let env_i = Hashtbl.find env i in
       let env_ident = Hashtbl.find env ident in
       Hashtbl.replace reg ident (env_i);
       (* return the current value of the ident *)
       env_ident
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
    | Eslice (int1, int2, arg) ->
       begin
	 match arg with
	 | Aconst (VBit b) -> VBit b
	 | Aconst (VBitArray ar) -> VBitArray (Array.sub ar int1 int2)
	 | Avar i->
	    begin
	      match Hashtbl.find env i with
	      | VBit b -> VBit b
	      | VBitArray ar -> VBitArray (Array.sub ar int1 int2)
	    end
       end			    
    | Eselect (int, arg) ->
       begin
	 match arg with
	 | Aconst (VBit b) -> VBit b
	 | Aconst (VBitArray ar) -> VBit ar.(int)
	 | Avar i->
	    begin
	      match Hashtbl.find env i with
	      | VBit b -> VBit b
	      | VBitArray ar -> VBit ar.(int)
	    end
       end			    
  in
  let value = ex_exp expression in
  (* TODO: optimize here to avoid setting ident to its previous value in case
     of a register *)
  Hashtbl.replace env ident value

let execute p n =
  (* Add the variables *)
  Env.iter (fun ident tbit -> 
	    let vbit = match tbit with
	      | TBit -> VBit false
	      | TBitArray l -> VBitArray (Array.make l false)
	    in Hashtbl.add env ident vbit ) p.p_vars;

  (* Read the program to create the ram *)
  Ram.create p;

  (* Executor of the program *)
  let main_loop i =
    (* Step number *)
    print_endline ("Step " ^(string_of_int i));
    (* Fill the input variables *)
    List.iter ( fun ident ->
		let n = match (Env.find ident p.p_vars) with 
		  | TBit -> 1 | TBitArray n -> n 
		in
		let v = ask_value ident n in
		Hashtbl.replace env ident v
	      ) p.p_inputs ;
    (* Move the reg of the previous step into the env *)
    Hashtbl.iter (fun key value -> Hashtbl.replace env key value) reg;
    (* Evaluates the equations *)
    List.iter ex_eq p.p_eqs;
    (* Print out the output *)
    List.iter ( fun i_output ->
		print_endline
		  ("=> "^i_output^" = "^(print_value (Hashtbl.find env i_output)))
	      ) p.p_outputs;
  in
  if n >= 0 then
    for i = 1 to n do
      main_loop i
    done
  else
    begin
      let i = ref (1) in
      while true do
	main_loop !i;
	i := !i + 1;
      done
    end
