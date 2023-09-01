From Coq Require Import Strings.String.
From ExtLib Require Import Monad.
From JSON Require Import JSON Decode Encode.
Require Import Util.JSON.
Require Literal Typ.

Module Dest.

  (** Represents the destination of a Bril instruction *)
  Record t := {
    name : string;
    typ : Typ.t;
  }.

  Global Instance JDecode_Dest : JDecode t :=
    fun j =>
      name <- (get_string =<< member "dest" j) ;;
      typ <- (decode =<< member "type" j) ;;
      ret {| name := name; typ := typ |}.

End Dest.

(* Newtypes to prevent confusion between destinations and operands *)

(** Represents an argument to a Bril instruction *)
Variant operand := Op (op : string).

Global Instance JDecode_Operand : JDecode operand :=
  fun j => Op <$> get_string j.

(** Represents a Bril label *)
Variant label := Label (label : string).

Global Instance JDecode_Label : JDecode label :=
  fun j => Label <$> get_string j.

(** Represents the name of a Bril function *)
Variant function := Func (func : string).

Global Instance JDecode_Func : JDecode function :=
  fun j => Func <$> get_string j.

Definition decode_pair {T} `{JDecode T} : JDecode (T * T) :=
  fun j =>
    lst <- @decode (list T) decode__list j ;;
    match lst with
    | [x; y] => ret (x, y)
    | _ => inl "List does not have two elements"
    end.

(** Represents a Bril instruction *)
Variant t :=

  (* Arithmetic *)
  | Add (dest : Dest.t) (x : operand) (y : operand)
  | Mul (dest : Dest.t) (x : operand) (y : operand)
  | Sub (dest : Dest.t) (x : operand) (y : operand)
  | Div (dest : Dest.t) (x : operand) (y : operand)

  (* Comparison *)
  | Eq (dest : Dest.t) (x : operand) (y : operand)
  | Lt (dest : Dest.t) (x : operand) (y : operand)
  | Gt (dest : Dest.t) (x : operand) (y : operand)
  | Le (dest : Dest.t) (x : operand) (y : operand)
  | Ge (dest : Dest.t) (x : operand) (y : operand)

  (* Logic *)
  | Not (dest : Dest.t) (x : operand)
  | And (dest : Dest.t) (x : operand) (y : operand)
  | Or (dest : Dest.t) (x : operand) (y : operand)

  (* Control *)
  | Jmp (l : label)
  | Br (cond : operand) (t : label) (f : label)
  | Call (dest : option Dest.t) (func : function) (args : list operand)
  | Ret (val : option operand)

  (* Misc *)
  | Const (dest : Dest.t) (lit : Literal.t)
  | Id (dest : Dest.t) (x : operand)
  | Print (args : list operand)
  | Nop.

(** Extract the opcode from an instruction *)
Definition opcode (instr : Instruction.t) : string :=
  match instr with
  | Add _ _ _ => "add"
  | Mul _ _ _ => "mul"
  | Sub _ _ _ => "sub"
  | Div _ _ _ => "div"
  | Eq _ _ _ => "eq"
  | Lt _ _ _ => "lt"
  | Gt _ _ _ => "gt"
  | Le _ _ _ => "le"
  | Ge _ _ _ => "ge"
  | Not _ _ => "not"
  | And _ _ _ => "and"
  | Or _ _ _ => "or"
  | Const _ _ => "const"
  | Id _ _ => "id"
  | Call _ _ _ => "call"
  | Jmp _ => "jmp"
  | Br _ _ _ => "br"
  | Ret _ => "ret"
  | Print _ => "print"
  | Nop => "nop"
  end.

(** Extract the destination of an instruction *)
Definition dest (instr : t) : option Dest.t :=
  match instr with
  | Add dst _ _
  | Mul dst _ _
  | Sub dst _ _
  | Div dst _ _
  | Eq dst _ _
  | Lt dst _ _
  | Gt dst _ _
  | Le dst _ _
  | Ge dst _ _
  | Not dst _
  | And dst _ _
  | Or dst _ _
  | Const dst _
  | Id dst _ => Some dst
  | Call dst _ _ => dst
  | Jmp _ | Br _ _ _ | Ret _ | Print _ | Nop => None
  end.

(** Extract the arguments to an instruction *)
Definition args (instr : t) : list operand :=
  match instr with
  | Add _ x y
  | Mul _ x y
  | Sub _ x y
  | Div _ x y
  | Eq _ x y
  | Lt _ x y
  | Gt _ x y
  | Le _ x y
  | Ge _ x y
  | And _ x y
  | Or _ x y => [x; y]
  | Not _ x | Id _ x | Br x _ _ | Ret (Some x) => [x]
  | Call _ _ args | Print args => args
  | Jmp _ | Const _ _ | Nop | Ret None => []
  end.

(** Extract the names of functions referenced by an instruction *)
Definition funcs (instr : Instruction.t) : list function :=
  if instr is Call _ func _ then [func] else [].

(** Extract the names of labels referenced by an instruction *)
Definition labels (instr : Instruction.t) : list label :=
  match instr with
  | Jmp l => [l]
  | Br _ t f => [t; f]
  | _ => []
  end.

(** If [value] an instruction [const lit], return [inr lit].
    Otherwise, return an error. *)
Definition value (instr : Instruction.t) : option Literal.t :=
  if instr is Const _ lit then Some lit else None.

(** Attempt to decode exactly two operands in list format from
    a JSON *)
Definition decode_op_pair j : string + (operand * operand) :=
  @decode (operand * operand) decode_pair j.

Import MonadLetNotation.

(** [decode_binary op j] is [inr (op dst x y)] where 
    [dst] is the destination decoded from [j], and [x], [y]
    are the two operands decoded from [j]. If [j] is malformed, 
    return an error. *)
Definition decode_binary (op : Dest.t -> operand -> operand -> t) j : string + t :=
  let* dest := decode j in
  let* (x, y) := decode_op_pair =<< member "args" j in
  ret (op dest x y).

(** Decode the element of a singleton json array *)
Definition decode_singleton {T} `{JDecode T} j : string + T :=
  lst <- @decode (list T) decode__list j ;;
  match lst with
  | [h] => ret h
  | _ => inl "Expected singleton array"
  end.

(** [decode_unary op j] is [inr (op dst x)] where 
    [dst] is the destination decoded from [j], and [x]
    is the single operand decoded from [j] *)
Definition decode_unary (op : Dest.t -> operand -> t) j : string + t :=
  let* dest := decode j in
  let* arg := decode_singleton =<< member "args" j in
  ret (op dest arg).

(** Decode a [const] instruction *)
Definition decode_const j : string + t :=
  let* dest := decode j in
  let* lit := decode =<< member "value" j in
  ret (Const dest lit).

(** Decode a [call] instruction *)
Definition decode_call j : string + t :=
  let* func := decode_singleton =<< member "funcs" j in
  let* args := @decode (list operand) decode__list =<< member "args" j in
  match decode j with
  | inr dest => ret (Call (Some dest) func args)
  | inl _ => ret (Call None func args)
  end.

(** Decode a [jmp] instruction *)
Definition decode_jmp j : string + t :=
  Jmp <$> (decode_singleton =<< member "labels" j).

(** Decode a [br] instruction *)
Definition decode_br j :=
  let* cond := decode_singleton =<< member "args" j in
  let* labels := member "labels" j in
  let* (t, f) := @decode (label * label) decode_pair labels in
  ret (Br cond t f).

(** Decode a [rets] instruction *)
Definition decode_ret j : string + t :=
  let* args := @decode (list operand) decode__list =<< member "args" j in
  match args with
  | [value] => ret (Ret (Some value))
  | [] => ret (Ret None)
  | _ => inl "Too many arguments to return"
  end.

(** Decode a [print] instruction *)
Definition decode_print j :=
  let* args := @decode (list operand) decode__list =<< member "args" j in
  ret (Print args).

Global Instance JDecode_Typ : JDecode t :=
  fun j =>
    op <- (get_string =<< member "op" j) ;;
    match op with
    | "add" => decode_binary Add j
    | "mul" => decode_binary Mul j
    | "sub" => decode_binary Sub j
    | "div" => decode_binary Div j
    | "eq" => decode_binary Eq j
    | "lt" => decode_binary Lt j
    | "gt" => decode_binary Gt j
    | "le" => decode_binary Le j
    | "ge" => decode_binary Ge j
    | "not" => decode_unary Not j
    | "and" => decode_binary And j
    | "or" => decode_binary Or j
    | "jmp" => decode_jmp j
    | "br" => decode_br j
    | "call" => decode_call j
    | "ret" => decode_ret j
    | "const" => decode_const j
    | "id" => decode_unary Id j
    | "print" => decode_print j
    | "nop" => ret Nop
    | _ => inl "Invalid opcode"
    end.

