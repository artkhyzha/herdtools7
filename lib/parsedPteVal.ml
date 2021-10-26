(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

type item =
  | OA of OutputAddress.t
  | KV of string * string
  | A of string list

type t = item list

let pp_item pp_oa = function
  | OA oa -> sprintf "oa:%s" (pp_oa oa)
  | KV (k,v) -> sprintf "%s:%s" k v
  | A xs -> sprintf "attrs:(%s)" (String.concat "," xs)

let mk_pp pp_oa p =
  sprintf "(%s)" (String.concat "," (List.map (pp_item pp_oa) p))

let pp_old = mk_pp OutputAddress.pp_old
and pp = mk_pp OutputAddress.pp
