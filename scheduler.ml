open Netlist_ast
open Graph
exception Combinational_cycle

let read_arg arg = match arg with
  | Avar s -> [s]
  | Aconst _ -> []

let read_exp eq = 
  let ident, exp = eq in
  match exp with
  | Earg a			-> read_arg a
  | Ereg ident			-> []
  | Enot a			-> read_arg a
  | Ebinop (_, a1, a2)		-> read_arg a1 @ read_arg a2
  | Emux (a1, a2, a3)		-> read_arg a1 @ read_arg a2 @ read_arg a3
  | Erom (_, _, a)		-> read_arg a
  | Eram (_, _, a1, a2, a3, a4)	-> 
     read_arg a1 @ read_arg a2 @ read_arg a3
  | Econcat (a1, a2)		-> read_arg a1 @ read_arg a2
  | Eslice (_, _, a)		-> read_arg a
  | Eselect (_, a)              -> read_arg a

let schedule p =
  let find ident =
    try
     Some (List.find (fun eq -> (fst eq) = ident) p.p_eqs)
    with Not_found -> (* Try to search in the input values *)
      let _ = List.find (fun i -> i=ident ) p.p_inputs in
      None
  in

  let graph = mk_graph () in
  
  (* Adding all vars to the graph *)
  Env.iter (fun ident _ -> add_node graph ident) p.p_vars;
  
  (* Adding all the edge to the graph *)
  List.iter ( fun eq -> 
	      let linked_to = read_exp eq in
	      List.iter (add_edge graph (fst eq)) linked_to;
	    ) p.p_eqs;

  assert (not (Graph.has_cycle graph));
  
  (* topological sort *)
  let sorted_ident = topological graph in
  (* Retrieve the equations from the ident *)
  let sorted_equations =
    List.fold_left
      ( fun eq_ls id ->
	match find id with
	| Some (ident, Ereg regIdent) -> eq_ls @ [(ident, Ereg regIdent)]
	| Some((_, _) as eq) -> eq :: eq_ls
	| None -> eq_ls
      ) [] sorted_ident      
  in
  
  let program = { p_eqs = sorted_equations;
		  p_inputs = p.p_inputs;
		  p_outputs = p.p_outputs;
		  p_vars = p.p_vars
		} in
  program
