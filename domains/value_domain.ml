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
			let fmt = Format.formatter_of_out_channel outc in
			match n with
				| Empty -> Format.fprintf fmt "bot"
				| Nb(n) -> Format.fprintf fmt "{%a}" Z.pp_print n
				| All -> Format.fprintf fmt "]-oo;+oo["
end

