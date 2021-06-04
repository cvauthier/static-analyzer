(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Antoine Miné 2015
  Marc Chevalier 2018
  Josselin Giet 2021
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)


(*
  Command line parsing utility
*)

module IMap = Map.Make(String)

open Arg

(* Flags *)

let silent  = ref false

(* string arguments *)

let file = ref ""
let cfg_out = ref ""
let domain_type = ref ""

let args = [
  "--silent", Set silent,
    " run the analyzer in silent mode (print only errors and node invariants)";
  "--dot-out", Set_string cfg_out,
    " print the cfg in this file (default is cfg.dot)";
	"--domain", Set_string domain_type,
		" abstract domain to use (default is interval)";
] |> align

let usage = "usage: ./analyzer.exe [options] filename.c"

let init () =
  let _ = parse args ( (:=) file) usage in
  let _ = if Filename.extension !file <> ".c" then
      let _ = Printf.eprintf "filename should have extension .c\n" in
      let _ = Arg.usage args usage in exit 0
  in
  let _ = if !cfg_out = "" then
      cfg_out := "cfg.dot" in
  let _ = if Filename.extension !cfg_out <> ".dot" then
      let _ = Printf.eprintf
          "CFG output file should have extension .dot (%s)\n" !cfg_out in
      let _ = Arg.usage args usage in exit 0
  in ()

