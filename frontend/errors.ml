(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Antoine Miné 2015
  Marc Chevalier 2018
  Josselin Giet 2021
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(*
  Error handling utility
*)

open Abstract_syntax_tree

type error_kind =
  | DivisionByZero
  | ModuloByZero
  | AssertFalse

let pp_error_kind fmt ek =
  Printf.fprintf fmt
  (match ek with
  | DivisionByZero    -> "DivisionByZero"
  | ModuloByZero      -> "ModuloByZero"
  | AssertFalse       -> "AssertFalse")

type err = error_kind * string * extent

let pp_error fmt (ek, msg, ext) =
  Printf.fprintf fmt "ERROR: %a at %s: %s\n"
    pp_error_kind ek
    (Cfg_printer.string_of_extent ext)
    msg
