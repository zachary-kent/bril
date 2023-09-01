Require Import Coq.Strings.String.
From ExtLib Require Import Monad.
From JSON Require Import JSON Decode Encode.

Require Import Util.JSON.
Require Typ.

(** Represents an argument to a Bril function *)
Record t := {
  (** The name of the argument *)
  name : string;
  (** The Bril type of the argument*)
  typ : Typ.t;
}.

Global Instance JDecode_Argument : JDecode t :=
  fun j =>
    name <- (get_string =<< member "name" j) ;;
    typ <- (decode =<< member "type" j) ;;
    ret {| name := name; typ := typ |}.

Global Instance JEncode_Argument : JEncode t :=
  fun arg =>
    JSON__Object
      [
        ("name", encode arg.(name));
        ("type", encode arg.(typ))
      ].
