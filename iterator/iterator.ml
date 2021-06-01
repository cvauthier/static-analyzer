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

	let iterate _ = ()

end
