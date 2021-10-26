(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
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

module Make(S:Scalar.S) =
  struct
    type scalar = S.t
    type pteval = AArch64PteVal.t
    type cst = (scalar,pteval) Constant.t

    let shift_address_right s c =
      let open Constant in
      if S.equal (S.of_int 12) c then Some (Symbolic (System (TLB,s)))
      else None

    let mask_valid = S.one
    let mask_db = S.shift_left S.one 7
    let mask_af = S.shift_left S.one 10
    let mask_dbm = S.shift_left S.one 51
    let mask_all_neg =
      S.lognot
        (S.logor
           (S.logor mask_valid  mask_db)
           (S.logor  mask_af  mask_dbm))
      
    let is_zero v = S.equal S.zero v
    let is_set v m = not (is_zero (S.logand v m))

    open AArch64PteVal

    let orop p m =
      if is_set m  mask_all_neg then None
      else
        let p = if is_set m mask_valid then { p with valid=1; } else p in
        let p = if is_set m mask_db then { p with db=0; } else p in
        let p = if is_set m mask_af then { p with af=1; } else p in
        let p = if is_set m mask_dbm then { p with dbm=1; } else p in
        Some p

    and andnot2 p m =
      if is_set m  mask_all_neg then None
      else
        let p = if is_set m mask_valid then { p with valid=0; } else p in
        let p = if is_set m mask_db then { p with db=1; } else p in
        let p = if is_set m mask_af then { p with af=0; } else p in
        let p = if is_set m mask_dbm then { p with dbm=0; } else p in
        Some p

  end
