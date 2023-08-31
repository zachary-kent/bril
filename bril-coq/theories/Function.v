Require Import Coq.Strings.String.

Require
  Argument
  BasicBlock
  Instruction.

(** Represents a Bril function *)
Record t := {
  (** The name of the Bril function *)
  name : string;
  (** Names of variables passed as arguments to the Bril function *)
  args : list Argument.t;
  (** The return type of the Bril function, or [None] if the Bril function has
      a void return type *)
  typ : option Typ.t;
  (** The instructions of the Bril function, partitioned into basic blocks *)
  blocks : list BasicBlock.t;
}.
