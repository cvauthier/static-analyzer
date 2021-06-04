(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Antoine Miné 2015
  Marc Chevalier 2018
  Josselin Giet 2021
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(*
  Simple driver: parses the file given as argument and prints it back.

  You should modify this file to call your functions instead!
*)

let select_value_domain (dname : string) : (module Value_domain.VALUE_DOMAIN) =
	match dname with 
		| "interval" | "" -> (module Value_domain.Interval)
		| "constant" -> (module Value_domain.Constant)
		| _ -> (Printf.eprintf "Unrecognized domain"; exit 0) 
	
(* parse filename *)
let doit filename =
	let vdom = select_value_domain !Options.domain_type in
	let module Vdom = (val vdom) in
	let module Dom = Domain.Make(Vdom) in
	let module Iter = Iterator.Make(Dom) in
  let prog = File_parser.parse_file filename in
  let cfg = Tree_to_cfg.prog prog in
  Printf.printf "%a" Cfg_printer.print_cfg cfg;
  Cfg_printer.output_dot !Options.cfg_out cfg;
  Iter.iterate cfg


(* parses arguments to get filename *)
let main () =
  let _ = Options.init () in
  doit !Options.file

let _ = main ()
