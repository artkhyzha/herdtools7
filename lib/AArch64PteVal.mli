(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris, France.                                       *)
(*                                                                          *)
(* Copyright 2020-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Abstraction of page table entry (PTE) *)

module Attrs : sig
  type t
  val default : t
  val compare : t -> t -> int
  val eq : t -> t -> bool
  val pp : t -> string
  val as_list : t -> string list
  val of_list : string list -> t
end

type t = {
  oa : OutputAddress.t;
  valid : int;
  af : int;
  db : int;
  dbm : int;
  el0 : int;
  attrs : Attrs.t;
  }
(* Accessors, setters *)
val is_af : t -> bool
val set_af : t -> t
val is_db : t -> bool
val set_db : t -> t
val is_dbm : t -> bool
val is_valid : t -> bool
val is_el0 : t -> bool
val get_oa : t -> OutputAddress.t
val same_oa : t -> t -> bool
val writable : bool -> bool -> t -> bool
val get_attrs : t -> string list
  
(* Default value *)
val prot_default : t (* Fields only *)
val default : string -> t (* Physical address + default fields *)
val of_pte : string -> t (* Default value for pte page table entry *)

(* Set oa  field *)
val set_oa : t -> string -> t

(* Flags have default values *)
val is_default : t -> bool

(* Finish parsing *)
val tr : ParsedPteVal.t -> t

(* Pretty print pp [hexa]  *)
val pp : bool -> t -> string  (* Default field not printed *)
val pp_v : t -> string  (* Decimal *)
val pp_hash : t -> string (* Backward compatibility for test hashes *)

val compare : t -> t -> int
val eq : t -> t -> bool
