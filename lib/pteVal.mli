(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris, France.                                       *)
(*                                                                          *)
(* Copyright 2021-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Abstract page table entry values *)
module type S = sig
  type t

  (* Default pte for virtual addresses and pte themselves  *)
  val default : string -> t
  val of_pte : string -> t

  val pp : bool -> t -> string
  val pp_v : t -> string
  val pp_hash : t -> string
  val tr : ParsedPteVal.t -> t

  val eq : t -> t -> bool
  val compare : t -> t -> int

  (* Access flag *)
  val is_af : t -> bool
  val set_af : t -> t

  (* Dirty bit *)
   val is_db : t -> bool
   val set_db : t -> t
   val is_dbm : t -> bool

  (* Valid *)
   val is_valid : t -> bool

  (* User level *)
   val is_el0 : t-> bool

   (* Output Address *)
   val get_oa : t -> OutputAddress.t
   val same_oa : t -> t -> bool

   (* Pte specifies a writable page, booleans are AARch64 specific *)
   val writable : bool -> bool -> t -> bool

   (* Attributes *)
   val get_attrs : t -> string list
     
end

module No : S
