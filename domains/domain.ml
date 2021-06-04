(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Antoine Miné 2015
  Marc Chevalier 2018
  Josselin Giet 2021
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(*
  Signature of abstract domains representing sets of envrionments
  (for instance: a map from variable to their bounds).
 *)

open! Cfg

module type DOMAIN =
  sig

    (* type of abstract elements *)
    (* an element of type t abstracts a set of mappings from variables
       to integers
     *)
    type t

    (* initial environment, with all variables initialized to 0 *)
    val init: var list -> t

    (* empty set of environments *)
    val bottom: t

    (* assign an integer expression to a variable *)
    val assign: t -> var -> int_expr -> t

    (* filter environments to keep only those satisfying the boolean expression *)
    val guard: t -> bool_expr -> t

    (* abstract join *)
    val join: t -> t -> t

    (* widening *)
    val widen: t -> t -> t

    (* whether an abstract element is included in another one *)
    val subset: t -> t -> bool

    (* whether the abstract element represents the empty set *)
    val is_bottom: t -> bool

    (* prints *)
    val print: out_channel -> t -> unit

  end

module Make(V: Value_domain.VALUE_DOMAIN) : DOMAIN =
struct
	
	exception Undeclared_variable

  type t = V.t VarMap.t

  let init: var list -> t = 
		let rec aux l = match l with
			| [] -> VarMap.empty
			| v::l -> VarMap.add v (V.const Z.zero) (aux l) in
		aux

	let bottom: t = VarMap.empty

	let assign: t -> var -> int_expr -> t = fun env variable expr ->
		let rec aux = function
			| CFG_int_unary(op,expr) -> V.unary (aux expr) op
			| CFG_int_binary(op,expr1,expr2) -> V.binary (aux expr1) (aux expr2) op
			| CFG_int_var(v) -> (try VarMap.find v env with Not_found -> raise Undeclared_variable)
			| CFG_int_const(n) -> V.const n
			| CFG_int_rand(n,m) -> V.rand n m in
		VarMap.add variable (aux expr) env

  let guard: t -> bool_expr -> t = fun env expr ->
		let rec aux = function
			| CFG_bool_unary(op,expr) -> begin match op with
					| AST_NOT -> let res1,res2 = aux expr in res2,res1
				end
			| CFG_bool_binary(op,expr1,expr2) -> begin 
					let f_and = VarMap.map2 (fun _ n m -> V.meet n m) and
							f_or = VarMap.map2 (fun _ n m -> V.join n m) and
							res1,res2 = aux expr1 and 
							res3,res4 = aux expr2 in
					match op with
  					| AST_AND -> f_and res1 res3, f_or res2 res4
  					| AST_OR  -> f_or res1 res3, f_and res2 res4
				end 
  		| CFG_compare(op,expr1,expr2) -> begin
					let rec aux2 = function
						| CFG_int_unary(op,expr) -> begin
								let value,f = aux2 expr in 
								(V.unary value op),(fun restr_v env -> f (V.bwd_unary value op restr_v) env)
							end
						| CFG_int_binary(op,expr1,expr2) -> begin
								let value1,f1 = aux2 expr1 and value2,f2 = aux2 expr2 in
								(V.binary value1 value2 op),(fun restr_v env -> let v1,v2 = V.bwd_binary value1 value2 op restr_v in f1 v1 (f2 v2 env))
							end
						| CFG_int_var(v) -> begin
								let value = try VarMap.find v env with Not_found -> raise Undeclared_variable in
								value, (fun restr_v env -> VarMap.add v restr_v env)
							end
						| CFG_int_const(n) -> (V.const n), (fun _ env -> env)
						| CFG_int_rand(n,m) -> (V.rand n m), (fun _ env -> env) in
					let op2 = match op with
						| AST_EQUAL -> Abstract_syntax_tree.AST_NOT_EQUAL | AST_NOT_EQUAL -> AST_EQUAL
  					| AST_LESS -> AST_GREATER_EQUAL | AST_GREATER_EQUAL -> AST_LESS
  					| AST_GREATER -> AST_LESS_EQUAL | AST_LESS_EQUAL -> AST_GREATER in
					let value1,f1 = aux2 expr1 and value2,f2 = aux2 expr2 in
					let v1,v2 = V.compare value1 value2 op and v3,v4 = V.compare value1 value2 op2 in
					(f1 v1 (f2 v2 env)), (f1 v3 (f2 v4 env))
				end
			| CFG_bool_const(b) -> let empty = VarMap.map (fun _ -> V.bottom) env in if b then env,empty else empty,env 
			| CFG_bool_rand -> env,env in
		fst (aux expr)

  let join: t -> t -> t = fun env1 env2 -> 
		VarMap.map2o (fun _ n -> n) (fun _ m -> m) (fun _ n m -> V.join n m) env1 env2

  let widen: t -> t -> t = fun env1 env2 ->
		VarMap.map2o (fun _ n -> n) (fun _ m -> m) (fun _ n m -> V.widen n m) env1 env2

  let subset: t -> t -> bool = fun env1 env2 -> 
		VarMap.for_all2o (fun _ _ -> false) (fun _ _ -> true) (fun _ v1 v2 -> V.subset v1 v2) env1 env2 

  let is_bottom: t -> bool = fun env -> VarMap.is_empty env || VarMap.exists (fun _ n -> V.is_bottom n) env

	let print: out_channel -> t -> unit = fun outc env ->
		VarMap.iter (fun variable value -> Printf.fprintf outc "%a=%a; " Cfg_printer.print_var variable V.print value) env

end

