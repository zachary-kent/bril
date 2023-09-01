From ExtLib Require Import Monad.
From JSON Require Import JSON Decode Encode.
Require Import Util.JSON.

(** Represents the type of a Bril value *)
Variant t := Int | Bool.

Global Instance JDecode_Typ : JDecode t :=
  fun j =>
    match j with
    | JSON__String "int" => ret Int
    | JSON__String "bool" => ret Bool
    | _ => inl "Cannot decode JSON"
    end.

Global Instance JEncode_Typ : JEncode t :=
  fun typ =>
    match typ with
    | Int => JSON__String "int"
    | Bool => JSON__String "bool"
    end.
