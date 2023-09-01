Require Import Coq.Numbers.BinNums.
From ExtLib Require Import Monad.
From JSON Require Import JSON Decode Encode.

(** Represents a literal Bril constant *)
Variant t :=
  (** A literal integer *)
  | Int (i : Z)
  (** A literal boolean *)
  | Bool (b : bool).

Global Instance JDecode_Literal : JDecode t :=
  fun j =>
    match j with
    | JSON__Number i => ret (Int i)
    | JSON__True => ret (Bool true)
    | JSON__False => ret (Bool false)
    | _ => inl "Cannot decode JSON"
    end.

Global Instance JEncode_Literal : JEncode t :=
  fun typ =>
    match typ with
    | Int i => JSON__Number i
    | Bool true => JSON__True
    | Bool false => JSON__False
    end.
