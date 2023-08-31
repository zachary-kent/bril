Require Import Coq.Strings.String.

Require Typ.

(** Represents an argument to a Bril function *)
Record t := {
  (** The name of the argument *)
  name : string;
  (** The Bril type of the argument*)
  typ : Typ.t;
}.
