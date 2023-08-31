Require Import Coq.Numbers.BinNums.

(** Represents a literal Bril constant *)
Variant t :=
  (** A literal integer *)
  | Int (i : Z)
  (** A literal boolean *)
  | Bool (b : bool).
