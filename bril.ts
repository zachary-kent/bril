export type Ident = string;

export const enum OpCode {
  add = "add",
  id = "id",
}

export interface Operation {
  op: OpCode;
  args: Ident[];
  dest: Ident;
}

export type ConstValue = number;

export interface Const {
  value: ConstValue;
  dest: Ident;
}

export type Instruction = Operation | Const;

export interface Function {
  name: Ident;
  instrs: Instruction[];
}

export interface Program {
  functions: Function[];
}
