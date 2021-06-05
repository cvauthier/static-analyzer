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

	type func_env = { fenv_func_name : string;
										fenv_entry_arc : arc option;
										fenv_val_map : D.t NodeHash.t;
										fenv_failed_asserts : unit ArcHash.t;
										fenv_children : func_env ArcHash.t; }

	type env = { env_cfg : cfg;
							 env_init_val_map : D.t NodeHash.t;
							 env_widen_nodes : unit NodeHash.t; 
							 env_called_funcs : unit FuncHash.t;
							 env_main_fenv : func_env; 
							 env_curr_fenv : func_env; }
	
	let find_widen_nodes env f =
		let visited = NodeHash.create 10 in
		let rec aux node =
			match NodeHash.find_opt visited node with
				| Some(true) -> ()
				| Some(false) -> NodeHash.add env.env_widen_nodes node ()
				| None -> begin
						NodeHash.add visited node false;
						List.iter (fun arc -> aux arc.arc_dst) node.node_out;
						NodeHash.replace visited node true
					end
		in aux f.func_entry

	let init_fenv f entry_arc =
		{ fenv_func_name = f.func_name; fenv_entry_arc = entry_arc; fenv_val_map = NodeHash.create 10; fenv_failed_asserts = ArcHash.create 10; fenv_children = ArcHash.create 10; }

	let rec compute_value env arc =
		let fenv = env.env_curr_fenv in
		try begin
			let src_val = NodeHash.find fenv.fenv_val_map arc.arc_src in
			match arc.arc_inst with
  			| CFG_skip(_) -> src_val
				| CFG_assign(var,expr) -> D.assign src_val var expr
				| CFG_guard(expr) -> D.guard src_val expr 
  			| CFG_assert(expr) -> begin
						let res = D.guard src_val expr in
						if not (D.subset src_val res) then ArcHash.replace fenv.fenv_failed_asserts arc ();
						src_val
					end
  			| CFG_call(f) -> begin
						if not (ArcHash.mem fenv.fenv_children arc) then
							(ArcHash.add fenv.fenv_children arc (init_fenv f (Some(arc))));
						iterate_function { env with env_curr_fenv = ArcHash.find fenv.fenv_children arc } src_val f
					end
		end
		with Not_found -> D.bottom

	and iterate_function env entry_val f =

		if FuncHash.mem env.env_called_funcs f then
			raise Recursive_call;
		
		let fenv = env.env_curr_fenv in
		FuncHash.add env.env_called_funcs f ();

		let worklist = ref (List.map (fun arc -> arc.arc_dst) f.func_entry.node_out) in
		NodeHash.replace fenv.fenv_val_map f.func_entry entry_val;
		
		while !worklist <> [] do

			let new_worklist = ref [] in
			List.iter (fun node ->
				
				let new_val = List.fold_left D.join D.bottom (List.map (compute_value env) node.node_in) in
				let new_val,prev_val = match NodeHash.find_opt fenv.fenv_val_map node with
					| None -> new_val,D.bottom
					| Some(prev_val) -> (if (NodeHash.mem env.env_widen_nodes node) then D.widen else D.join) prev_val new_val, prev_val in

				NodeHash.replace fenv.fenv_val_map node new_val;
				
				if not (D.subset new_val prev_val) then begin
					List.iter (fun arc -> new_worklist := arc.arc_dst::(!new_worklist)) node.node_out
				end

			) !worklist;
			worklist := !new_worklist
		done;
		FuncHash.remove env.env_called_funcs f;
		match NodeHash.find_opt fenv.fenv_val_map f.func_exit with
			| Some(v) -> v
			| None -> D.bottom
	
	let print_results env = 
		let print_val_map tabs vmap = 
			NodeHash.iter (fun node value ->
				Printf.printf "%s%d : abstract invariant %a\n" tabs node.node_id D.print value) vmap in
		
		let rec print_fenv tabs fenv = 
			Printf.printf "%sFunction %s " tabs fenv.fenv_func_name;
			(match fenv.fenv_entry_arc with
				| None -> ()
				| Some(arc) -> Printf.printf "(entry arc : %d)" arc.arc_id);
			Printf.printf "\n\n";

			print_val_map tabs fenv.fenv_val_map;
			Printf.printf "\n";

			if (ArcHash.length fenv.fenv_failed_asserts) <> 0 then begin
				Printf.printf "%sSome assertions failed : \n" tabs;
				ArcHash.iter (fun arc () -> Printf.printf "%sArc %d : %a\n" tabs arc.arc_id Cfg_printer.print_inst arc.arc_inst) fenv.fenv_failed_asserts;
				Printf.printf "\n";
			end;

			ArcHash.iter (fun _ fenv2 -> print_fenv (tabs^"\t") fenv2) fenv.fenv_children in

		Printf.printf "Initialization : \n\n";
		print_val_map "" env.env_init_val_map;
		Printf.printf "\n";
		print_fenv "" env.env_main_fenv

	let iterate cfg = 
		let main_func = List.find (fun f -> f.func_name = "main") cfg.cfg_funcs in
		let fenv = init_fenv main_func None in
		let env = { env_cfg = cfg; 
								env_init_val_map = NodeHash.create 10; 
								env_widen_nodes = NodeHash.create 10; 
								env_called_funcs = FuncHash.create 10;
								env_main_fenv = fenv;
								env_curr_fenv = fenv; } in
		
		(* iterate on initialization block *)
		let curr = ref (D.init cfg.cfg_vars) in
		let init_entry = ref cfg.cfg_init_entry in

		NodeHash.replace env.env_init_val_map !init_entry !curr;
		while (!init_entry).node_id <> cfg.cfg_init_exit.node_id do
			let arc = List.hd (!init_entry).node_out in
			(match arc.arc_inst with
				| CFG_assign(var,expr) -> curr := D.assign !curr var expr 
				| _ -> failwith "error");
			init_entry := arc.arc_dst;
			NodeHash.replace env.env_init_val_map !init_entry !curr;
		done;
		
		(* iterate on main function *)
		List.iter (find_widen_nodes env) cfg.cfg_funcs;
		let _ = iterate_function env !curr main_func in
		print_results env

end
