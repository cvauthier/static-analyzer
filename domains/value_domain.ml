(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Antoine Miné 2015
  Marc Chevalier 2018
  Josselin Giet 2021
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(*
  Signature of abstract domains representing sets of integers
  (for instance: constants or intervals).
 *)

open Abstract_syntax_tree

module type VALUE_DOMAIN =
  sig

    (* type of abstract elements *)
    (* an element of type t abstracts a set of integers *)
    type t

    (* unrestricted value: [-oo,+oo] *)
    val top: t

    (* bottom value: empty set *)
    val bottom: t

    (* constant: {c} *)
    val const: Z.t -> t

    (* interval: [a,b] *)
    val rand: Z.t -> Z.t -> t


    (* unary operation *)
    val unary: t -> int_unary_op -> t

    (* binary operation *)
    val binary: t -> t -> int_binary_op -> t


    (* comparison *)
    (* [compare x y op] returns (x',y') where
       - x' abstracts the set of v  in x such that v op v' is true for some v' in y
       - y' abstracts the set of v' in y such that v op v' is true for some v  in x
       i.e., we filter the abstract values x and y knowing that the test is true

       a safe, but not precise implementation, would be:
       compare x y op = (x,y)
     *)
    val compare: t -> t -> compare_op -> (t * t)


    (* backards unary operation *)
    (* [bwd_unary x op r] return x':
       - x' abstracts the set of v in x such as op v is in r
       i.e., we fiter the abstract values x knowing the result r of applying
       the operation on x
     *)
    val bwd_unary: t -> int_unary_op -> t -> t

     (* backward binary operation *)
     (* [bwd_binary x y op r] returns (x',y') where
       - x' abstracts the set of v  in x such that v op v' is in r for some v' in y
       - y' abstracts the set of v' in y such that v op v' is in r for some v  in x
       i.e., we filter the abstract values x and y knowing that, after
       applying the operation op, the result is in r
      *)
    val bwd_binary: t -> t -> int_binary_op -> t -> (t * t)


    (* set-theoretic operations *)
    val join: t -> t -> t
    val meet: t -> t -> t

    (* widening *)
    val widen: t -> t -> t

    (* subset inclusion of concretizations *)
    val subset: t -> t -> bool

    (* check the emptiness of the concretization *)
    val is_bottom: t -> bool

    (* print abstract element *)
		val print: out_channel -> t -> unit

end

module Constant : VALUE_DOMAIN = 
struct
	
	type t = All | Empty | Nb of Z.t
	
	let top: t = All
	let bottom: t = Empty
	
	let const: Z.t -> t = fun x -> Nb(x)
	let rand: Z.t -> Z.t -> t = fun x y -> if Z.equal x y then Nb(x) else if Z.gt x y then Empty else All

	let unary: t -> int_unary_op -> t = fun n op -> match (op,n) with
  	| (AST_UNARY_PLUS,n)  -> n
  	| (AST_UNARY_MINUS,Nb(n)) -> Nb(Z.sub Z.zero n)
		| (AST_UNARY_MINUS,n) -> n
	
	let meet: t -> t -> t = fun n m -> match n,m with
		| Empty,_ | _,Empty -> Empty
		| All,p | p,All -> p
		| Nb(n),Nb(m) -> if Z.equal n m then Nb(n) else Empty

	let binary: t -> t -> int_binary_op -> t = fun n m op -> match op with
		| AST_PLUS | AST_MINUS -> begin
				match n,m with
					| Empty,_ | _,Empty -> Empty
					| All,_ | _,All -> All
					| Nb(n),Nb(m) -> Nb((if op = AST_PLUS then Z.add else Z.sub) n m)
			end
		| AST_MULTIPLY -> begin
				match n,m with
					| Empty,_ | _,Empty -> Empty
					| All,Nb(n) when Z.equal n Z.zero -> Nb(Z.zero) 
					| Nb(n),All when Z.equal n Z.zero -> Nb(Z.zero)
					| _,All | All,_ -> All
					| Nb(n),Nb(m) -> Nb(Z.mul n m)
				end
		| AST_DIVIDE -> begin
				match n,m with
					| Empty,_ | _,Empty -> Empty
					| All,All -> All
					| All,Nb(n) -> if Z.equal n Z.zero then Empty else All
					| Nb(n),All -> if Z.equal n Z.zero then Nb(Z.zero) else All
					| Nb(n),Nb(m) -> if Z.equal m Z.zero then Empty else Nb(Z.div n m)
				end
		| AST_MODULO -> begin
				match n,m with
					| Empty,_ | _,Empty -> Empty
					| All,All -> All
					| All,Nb(n) -> if Z.equal n Z.zero then Empty else if Z.equal n Z.one then Nb(Z.zero) else All
					| Nb(n),All -> if Z.equal n Z.zero then Nb(Z.zero) else All
					| Nb(n),Nb(m) -> if Z.equal m Z.zero then Empty else Nb(Z.rem n m)
				end

	let compare: t -> t -> compare_op -> (t * t) = fun n m op -> 
		let aux n m op = match op with
			| AST_NOT_EQUAL -> begin match n,m with
					| n,All -> n
					| _,Empty | Empty,_ -> Empty
					| All,Nb(m) -> All
					| Nb(n),Nb(m) -> if Z.equal n m then Empty else Nb(n)
				end
			| AST_LESS | AST_LESS_EQUAL -> begin match n,m with
					| n,All -> n
					| _,Empty | Empty,_ -> Empty
					| All,Nb(m) -> All
					| Nb(n),Nb(m) -> if (if op = AST_LESS then Z.lt else Z.leq) n m then Nb(n) else Empty
				end 
			| _ -> Empty in
		match op with
			| AST_EQUAL -> let p = meet n m in (p,p) 
			| AST_NOT_EQUAL  -> (aux n m op, aux m n op) 
  		| AST_LESS -> (aux n m AST_LESS, aux m n AST_LESS_EQUAL)
  		| AST_LESS_EQUAL -> (aux n m AST_LESS_EQUAL, aux m n AST_LESS)
  		| AST_GREATER -> (aux m n AST_LESS, aux n m AST_LESS_EQUAL)
  		| AST_GREATER_EQUAL -> (aux m n AST_LESS_EQUAL, aux n m AST_LESS)

  let bwd_unary: t -> int_unary_op -> t -> t = fun x op r -> meet (unary r op) x
    
	let bwd_binary: t -> t -> int_binary_op -> t -> (t * t) = fun x y op r -> match op with
		| AST_PLUS -> (meet x (binary r y AST_MINUS), meet y (binary r x AST_MINUS))
		| AST_MINUS -> (meet x (binary r y AST_PLUS), meet y (binary x r AST_MINUS))
		| AST_MULTIPLY -> begin 
			let aux x y r = match x,y with
				| All,All -> All
				| Nb(x),All -> if Z.equal (Z.rem r x) Z.zero then Nb(x) else Empty
				| All,Nb(x) -> if Z.equal (Z.rem r x) Z.zero then Nb(Z.div r x) else Empty
				| Nb(x),Nb(y) -> if Z.equal (Z.mul x y) r then Nb(x) else Empty
				| Empty,_ | _,Empty -> Empty in
			match r with
				| All -> x,y
				| Empty -> Empty,Empty
				| Nb(r) -> (aux x y r, aux y x r)
			end
		| AST_DIVIDE -> begin match x,y,r with
				| _,_,Empty | _,Empty,_ | Empty,_,_ -> Empty,All
				| _,Nb(y),_ when Z.equal y Z.zero -> Empty,Empty
				| _,_,All -> x,y
				| All,All,Nb(r) -> if Z.equal r Z.zero then Nb(Z.zero),All else All,All
				| All,Nb(y),Nb(r) -> Nb(Z.mul y r),Nb(y)
				| Nb(x),Nb(y),Nb(r) -> if Z.equal (Z.div x y) r then Nb(x),Nb(y) else Empty,Empty 
				| Nb(x),All,Nb(r) -> Nb(x),All (* to refine ? *)
			end
		| AST_MODULO -> begin match x,y,r with
				| _,_,Empty | _,Empty,_ | Empty,_,_ -> Empty,All
				| _,Nb(y),_ when Z.equal y Z.zero -> Empty,Empty
				| _,_,All -> x,y
				| All,All,Nb(r) -> All,All
				| Nb(x),Nb(y),Nb(r) -> if Z.equal (Z.rem x y) r then Nb(x),Nb(y) else Empty,Empty 
				| _,_,_ -> x,y (* to refine ? *)
			end

	let join: t -> t -> t = fun n m -> match n,m with
		| All,_ | _,All -> All
		| Empty,p | p,Empty -> p
		| Nb(n),Nb(m) -> if Z.equal n m then Nb(n) else All

  let widen: t -> t -> t = join

  let subset: t -> t -> bool = fun n m -> match n,m with
		| _,All -> true
		| Nb(n),Nb(m) -> Z.equal n m
		| Empty,_ -> true
		| _ -> false

  let is_bottom: t -> bool = function
		| Empty -> true
		| All | Nb(_) -> false

  let print: out_channel -> t -> unit = fun outc n -> 
		match n with
			| Empty -> Printf.fprintf outc "bot"
			| Nb(n) -> Printf.fprintf outc "{%a}" Z.output n
			| All -> Printf.fprintf outc "[-oo;+oo]"

end

module Interval : VALUE_DOMAIN = 
struct
	
	exception Bad_bounds

	type bound = | Inf | NInf | Nb of Z.t
	type t = Empty | Interval of bound * bound
	
	let lt_bd: bound -> bound -> bool = fun b1 b2 -> match b1,b2 with
		| NInf,_ | _,Inf -> true
		| Inf,_ | _,NInf -> false
		| Nb(x),Nb(y) -> Z.lt x y
	
	let eq_bd: bound -> bound -> bool = fun b1 b2 -> match b1,b2 with
		| Inf,Inf -> true
		| NInf,NInf -> true
		| Nb(x),Nb(y) -> Z.equal x y
		| _ -> false

	let leq_bd: bound -> bound -> bool = fun b1 b2 -> (eq_bd b1 b2) || (lt_bd b1 b2)
	
	let max_bd: bound -> bound -> bound = fun b1 b2 -> if leq_bd b1 b2 then b2 else b1
	let min_bd: bound -> bound -> bound = fun b1 b2 -> if leq_bd b1 b2 then b1 else b2

	let umin_bd: bound -> bound = function
		| NInf -> Inf
		| Inf -> NInf
		| Nb(x) -> Nb(Z.sub Z.zero x)
	
	let add_bd: bound -> bound -> bound = fun b1 b2 -> match b1,b2 with
		| Inf,NInf | NInf,Inf -> raise Bad_bounds
		| Inf,_ | _,Inf -> Inf
		| NInf,_ | _,NInf -> NInf
		| Nb(x),Nb(y) -> Nb(Z.add x y)
	
	let mul_bd: bound -> bound -> bound = fun b1 b2 -> match b1,b2 with
		| Inf,Inf | NInf,NInf -> Inf
		| Inf,NInf | NInf,Inf -> NInf
		| Inf,Nb(x) | Nb(x),Inf -> if Z.lt Z.zero x then Inf else if Z.lt x Z.zero then NInf else Nb(Z.zero)
		| NInf,Nb(x) | Nb(x),NInf -> if Z.lt Z.zero x then NInf else if Z.lt x Z.zero then Inf else Nb(Z.zero)
		| Nb(x),Nb(y) -> Nb(Z.mul x y)
	
	let div_bd: bound -> bound -> bound = fun b1 b2 -> match b1,b2 with
		| Inf,Inf | Inf,NInf | NInf,Inf | NInf,NInf -> raise Bad_bounds
		| Inf,Nb(x) -> if Z.lt Z.zero x then Inf else if Z.lt x Z.zero then NInf else raise Bad_bounds
		| NInf,Nb(x) -> if Z.lt Z.zero x then NInf else if Z.lt x Z.zero then Inf else raise Bad_bounds
		| Nb(_),Inf -> Nb(Z.zero)
		| Nb(_),NInf -> Nb(Z.zero)
		| Nb(x),Nb(y) -> if Z.equal y Z.zero then raise Bad_bounds else Nb(Z.div x y)

	let make_interval: bound -> bound -> t = fun b1 b2 -> match b1,b2 with
		| Inf,_ | _,NInf -> Empty
		| Nb(x),Nb(y) when Z.lt y x -> Empty
		| _ -> Interval(b1,b2)

	let top: t = Interval(NInf,Inf)

	let bottom: t = Empty

	let const: Z.t -> t = fun x -> Interval(Nb(x),Nb(x))
	
	let rand: Z.t -> Z.t -> t = fun x y -> make_interval (Nb(x)) (Nb(y)) 

	let join: t -> t -> t = fun it1 it2 -> match it1,it2 with
		| it,Empty | Empty,it -> it
		| Interval(b1,b2), Interval(c1,c2) -> Interval(min_bd b1 c1, max_bd b2 c2)

  let meet: t -> t -> t = fun it1 it2 -> match it1,it2 with
		| _,Empty | Empty,_ -> Empty
		| Interval(b1,b2),Interval(c1,c2) -> make_interval (max_bd b1 c1) (min_bd b2 c2)

  let unary: t -> int_unary_op -> t = fun it op -> match op,it with
		| AST_UNARY_PLUS,it -> it
		| AST_UNARY_MINUS,Empty -> Empty
		| AST_UNARY_MINUS,Interval(b1,b2) -> Interval(umin_bd b2, umin_bd b1)

  let binary: t -> t -> int_binary_op -> t = fun it1 it2 op -> match it1,it2 with
		| Empty,_ | _,Empty -> Empty
		| Interval(b1,b2),Interval(c1,c2) -> begin 
				match op with
					| AST_PLUS -> Interval(add_bd b1 c1, add_bd b2 c2)
					| AST_MINUS -> Interval(add_bd c1 (umin_bd c2), add_bd b2 (umin_bd c1))
					| AST_MULTIPLY -> begin
							let zero = Nb(Z.zero) and
									min_one = Nb(Z.pred Z.zero) in
							let rec aux (b1,b2) (c1,c2) = match (b1,b2),(c1,c2) with
								| _ when leq_bd zero b1 && leq_bd zero c1 -> Interval(mul_bd b1 c1, mul_bd b2 c2)
								| _ when lt_bd b2 zero 										-> unary (aux (umin_bd b2, umin_bd b1) (c1,c2)) AST_UNARY_MINUS
								| _ when lt_bd b1 zero 										-> join (aux (b1,min_one) (c1,c2)) (aux (zero,b2) (c1,c2))
								| _																				-> aux (c1,c2) (b1,b2)
							in aux (b1,b2) (c1,c2)
						end
					| AST_DIVIDE | AST_MODULO -> begin
							let zero = Nb(Z.zero) and
									one = Nb(Z.one) and
									min_one = Nb(Z.pred Z.zero) in
							let rec aux (b1,b2) (c1,c2) = match (b1,b2),(c1,c2) with
								| _ when leq_bd zero b1 && lt_bd zero c1  -> begin
										if op = AST_MODULO then Interval(zero,add_bd c2 min_one)
										else Interval(div_bd b1 c2, div_bd b2 c1)
									end
								| _ when lt_bd b2 zero 										-> unary (aux (umin_bd b2, umin_bd b1) (c1,c2)) AST_UNARY_MINUS
								| _ when lt_bd c2 zero 										-> unary (aux (b1,b2) (umin_bd c2, umin_bd c1)) AST_UNARY_MINUS
								| _ when lt_bd b1 zero 										-> join (aux (b1,min_one) (c1,c2)) (aux (zero,b2) (c1,c2))
								| _ when leq_bd c1 zero && leq_bd zero c2 -> begin
										let res1 = if lt_bd zero c2 then aux (b1,b2) (one,c2) else Empty in
										let res2 = if lt_bd c1 zero then aux (b1,b2) (c2,min_one) else Empty in
										join res1 res2
									end
								| _																				-> raise Bad_bounds
							in aux (b1,b2) (c1,c2)
						end
			end

	let compare: t -> t -> compare_op -> (t * t) = fun n m op -> 
		let aux n m op = match op with
			| AST_NOT_EQUAL -> begin match n,m with
					| _,Empty | Empty,_ -> Empty
					| Interval(b1,b2),Interval(c1,c2) when eq_bd c1 c2 -> begin
							let d1 = if eq_bd c1 b1 then add_bd b1 (Nb(Z.one)) else b1 in
							let d2 = if eq_bd c1 b2 then add_bd b2 (Nb(Z.pred Z.zero)) else b2 in
							make_interval d1 d2
						end
					| n,_ -> n
				end
			| AST_LESS | AST_LESS_EQUAL -> begin match n,m with
					| _,Empty | Empty,_ -> Empty
					| n,Interval(_,c2) -> meet n (Interval(NInf,c2))
				end 
			| _ -> Empty in
		match op with
			| AST_EQUAL -> let p = meet n m in (p,p) 
			| AST_NOT_EQUAL  -> (aux n m op, aux m n op) 
  		| AST_LESS -> (aux n m AST_LESS, aux m n AST_LESS_EQUAL)
  		| AST_LESS_EQUAL -> (aux n m AST_LESS_EQUAL, aux m n AST_LESS)
  		| AST_GREATER -> (aux m n AST_LESS, aux n m AST_LESS_EQUAL)
  		| AST_GREATER_EQUAL -> (aux m n AST_LESS_EQUAL, aux n m AST_LESS)

	let bwd_unary: t -> int_unary_op -> t -> t = fun x op r -> meet x (unary r op)

  let bwd_binary: t -> t -> int_binary_op -> t -> (t * t) = fun it1 it2 op r -> match op with 
		| AST_PLUS -> (meet it1 (binary r it2 AST_MINUS), meet it2 (binary r it1 AST_MINUS))
		| AST_MINUS -> (meet it1 (binary r it2 AST_PLUS), meet it2 (binary it1 r AST_MINUS))
		| AST_MULTIPLY -> (meet it1 (binary r it2 AST_DIVIDE), meet it2 (binary r it1 AST_DIVIDE))
		| AST_DIVIDE | AST_MODULO -> it1,it2

  let widen: t -> t -> t = fun it1 it2 -> match it1,it2 with
		| Empty,it | it,Empty -> it
		| Interval(b1,b2),Interval(c1,c2) -> begin
				let d1 = if lt_bd c1 b1 then NInf else b1 in
				let d2 = if lt_bd b2 c2 then Inf else b2 in
				make_interval d1 d2
			end

  let subset: t -> t -> bool = fun it1 it2 -> match it1,it2 with
		| Empty,_ -> true
		| _,Empty -> false
		| Interval(b1,b2),Interval(c1,c2) -> (leq_bd c1 b1) && (leq_bd b2 c2)

	let is_bottom: t -> bool = function
	 	| Empty -> true
		| Interval(_,_) -> false

	let print: out_channel -> t -> unit = fun outc it ->
		let pp_bd outc = function
			| Inf -> Printf.fprintf outc "+oo"
			| NInf -> Printf.fprintf outc "-oo"
			| Nb(x) -> Printf.fprintf outc "%a" Z.output x in
		match it with
			| Empty -> Printf.fprintf outc "bot"
			| Interval(b1,b2) -> Printf.fprintf outc "[%a,%a]" pp_bd b1 pp_bd b2

end

