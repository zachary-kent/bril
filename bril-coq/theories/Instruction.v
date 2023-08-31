Require Import Coq.Strings.String.

Require Literal.

(** Represents a Bril instruction *)
Variant t :=

  (* Arithmetic *)
  | Add (dest : string) (x : string) (y : string)
  | Mul (dest : string) (x : string) (y : string)
  | Sub (dest : string) (x : string) (y : string)
  | Div (dest : string) (x : string) (y : string)

  (* Comparison *)
  | Eq (dest : string) (x : string) (y : string)
  | Lt (dest : string) (x : string) (y : string)
  | Gt (dest : string) (x : string) (y : string)
  | Le (dest : string) (x : string) (y : string)
  | Ge (dest : string) (x : string) (y : string)

  (* Logic *)
  | Not (dest : string) (x : string)
  | And (dest : string) (x : string) (y : string)
  | Or (dest : string) (x : string) (y : string)

  (* Control *)
  | Jmp (l : string)
  | Br (cond : string) (t : string) (f : string)
  | Call (dest : option string) (name : string) (args : list string)
  | Ret

  (* Misc *)
  | Const (dest : string) (lit : Literal.t)
  | Id (dest : string) (x : string)
  | Print (args : list string)
  | Nop.
