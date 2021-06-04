(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Antoine Miné 2015
  Marc Chevalier 2018
  Josselin Giet 2021
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

open! Cfg

module type ITERATOR =
  sig

    (* prints *)
		val iterate: cfg -> unit

  end

module Make(D: Domain.DOMAIN) : ITERATOR =
struct

	exception Recursive_call

	type env = { cfg : cfg; 
							 widen_nodes : unit NodeHash.t; 
							 val_map : D.t NodeHash.t;
							 failed_asserts : unit ArcHash.t }
	
	let find_widen_nodes env f =
		let visited = NodeHash.create 10 in
		let rec aux node =
			match NodeHash.find_opt visited node with
				| Some(true) -> ()
				| Some(false) -> NodeHash.add env.widen_nodes node ()
				| None -> begin
						NodeHash.add visited node false;
						List.iter (fun arc -> aux arc.arc_dst) node.node_out;
						NodeHash.replace visited node true
					end
		in aux f.func_entry

	let rec compute_value env arc =
		try begin
			let src_val = NodeHash.find env.val_map arc.arc_src in
			match arc.arc_inst with
  			| CFG_skip(_) -> src_val
				| CFG_assign(var,expr) -> D.assign src_val var expr
				| CFG_guard(expr) -> D.guard src_val expr 
  			| CFG_assert(expr) -> begin
						let res = D.guard src_val expr in
						if not (D.subset src_val res) then ArcHash.replace env.failed_asserts arc ();
						src_val
					end
  			| CFG_call(f) -> iterate_function env src_val f
		end
		with Not_found -> D.bottom

	and iterate_function env entry_val f =
		
		if NodeHash.mem env.val_map f.func_entry then
			raise Recursive_call;
		
		find_widen_nodes env f;

		let worklist = ref (List.map (fun arc -> arc.arc_dst) f.func_entry.node_out) in
		NodeHash.replace env.val_map f.func_entry entry_val;
		
		while !worklist <> [] do

			let new_worklist = ref [] in
			List.iter (fun node ->
				
				let new_val = List.fold_left D.join D.bottom (List.map (compute_value env) node.node_in) in
				let new_val,prev_val = match NodeHash.find_opt env.val_map node with
					| None -> new_val,D.bottom
					| Some(prev_val) -> (if (NodeHash.mem env.widen_nodes node) then D.widen else D.join) prev_val new_val, prev_val in

				NodeHash.replace env.val_map node new_val;
				
				if not (D.subset new_val prev_val) then begin
					List.iter (fun arc -> new_worklist := arc.arc_dst::(!new_worklist)) node.node_out
				end

			) !worklist;
			worklist := !new_worklist
		done;
		match NodeHash.find_opt env.val_map f.func_exit with
			| Some(v) -> v
			| None -> D.bottom
	
	let print_results env = 
		List.iter (fun node ->
			if NodeHash.mem env.val_map node then begin
				Printf.printf "%d : invariant %a\n" node.node_id D.print (NodeHash.find env.val_map node) 
			end
			else
				Printf.printf "%d : not reached\n" node.node_id
			) env.cfg.cfg_nodes;
		if (ArcHash.length env.failed_asserts) <> 0 then begin
			Printf.printf "Some assertions failed : \n";
			ArcHash.iter (fun arc () -> Printf.printf "Arc %d : %a" arc.arc_id Cfg_printer.print_inst arc.arc_inst) env.failed_asserts
		end 

	let iterate cfg = 
		let n = List.length cfg.cfg_nodes in
		let env = { cfg = cfg; widen_nodes = NodeHash.create 10; val_map = NodeHash.create n; failed_asserts = ArcHash.create 10 } in
		
		(* iterate on initialization block *)
		let curr = ref (D.init cfg.cfg_vars) in
		let init_entry = ref cfg.cfg_init_entry in

		NodeHash.replace env.val_map !init_entry !curr;

		while (!init_entry).node_id <> cfg.cfg_init_exit.node_id do
			let arc = List.hd (!init_entry).node_out in
			(match arc.arc_inst with
				| CFG_assign(var,expr) -> curr := D.assign !curr var expr 
				| _ -> failwith "error");
			init_entry := arc.arc_dst;
			NodeHash.replace env.val_map !init_entry !curr;
		done;
		
		(* iterate on main function *)
		let _ = iterate_function env !curr (List.find (fun f -> f.func_name = "main") cfg.cfg_funcs) in
		print_results env

end
