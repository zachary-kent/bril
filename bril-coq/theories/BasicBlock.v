Require Import Coq.Strings.String.
Require Instruction.

(** Represents a Bril basic block; that is, a series of consecutive instructions
    such that if any of them are executed, all of them are executed *)
Record t := {
  (** The label marking the beginning of the basic block, if any *)
  label : option string;
  (** The instructions in the basic block *)
  instrs : list Instruction.t;
}.
