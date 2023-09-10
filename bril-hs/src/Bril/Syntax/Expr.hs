module Bril.Syntax.Expr (Expr, Expr' (..), uses, opcode, isPure) where

import Bril.Syntax.Literal (Literal)
import Data.List (sort)
import Data.Text (Text)

data Expr' a
  = Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | FAdd a a
  | FSub a a
  | FMul a a
  | FDiv a a
  | Eq a a
  | Lt a a
  | Gt a a
  | Le a a
  | Ge a a
  | FEq a a
  | FLt a a
  | FGt a a
  | FLe a a
  | FGe a a
  | Not a
  | And a a
  | Or a a
  | Call Text [a]
  | Id a
  | Const Literal
  | Alloc a
  | Load a
  | PtrAdd a a
  deriving (Show, Functor, Foldable, Traversable)

type Expr = Expr' Text

instance (Ord a) => Eq (Expr' a) where
  Add x y == Add m n = sort [x, y] == sort [m, n]
  Mul x y == Mul m n = sort [x, y] == sort [m, n]
  Sub x y == Sub m n = [x, y] == [m, n]
  Div x y == Div m n = [x, y] == [m, n]
  FAdd x y == FAdd m n = sort [x, y] == sort [m, n]
  FMul x y == FMul m n = sort [x, y] == sort [m, n]
  FSub x y == FSub m n = [x, y] == [m, n]
  FDiv x y == FDiv m n = [x, y] == [m, n]
  Eq x y == Eq m n = sort [x, y] == sort [m, n]
  Lt x y == Lt m n = [x, y] == [m, n]
  Gt x y == Gt m n = [x, y] == [m, n]
  Le x y == Le m n = [x, y] == [m, n]
  Ge x y == Ge m n = [x, y] == [m, n]
  FEq x y == FEq m n = sort [x, y] == sort [m, n]
  FLt x y == FLt m n = [x, y] == [m, n]
  FGt x y == FGt m n = [x, y] == [m, n]
  FLe x y == FLe m n = [x, y] == [m, n]
  FGe x y == FGe m n = [x, y] == [m, n]
  Not x == Not y = x == y
  And x y == And m n = sort [x, y] == sort [m, n]
  Or x y == Or m n = sort [x, y] == sort [m, n]
  Id x == Id y = x == y
  _ == _ = False

uses :: Expr' a -> [a]
uses (Add x y) = [x, y]
uses (Sub x y) = [x, y]
uses (Mul x y) = [x, y]
uses (Div x y) = [x, y]
uses (FAdd x y) = [x, y]
uses (FSub x y) = [x, y]
uses (FMul x y) = [x, y]
uses (FDiv x y) = [x, y]
uses (Eq x y) = [x, y]
uses (Lt x y) = [x, y]
uses (Gt x y) = [x, y]
uses (Le x y) = [x, y]
uses (Ge x y) = [x, y]
uses (FEq x y) = [x, y]
uses (FLt x y) = [x, y]
uses (FGt x y) = [x, y]
uses (FLe x y) = [x, y]
uses (FGe x y) = [x, y]
uses (Not x) = [x]
uses (And x y) = [x, y]
uses (Or x y) = [x, y]
uses (Call _ args) = args
uses (Id x) = [x]
uses (Const _) = []
uses (Alloc x) = [x]
uses (Load x) = [x]
uses (PtrAdd base off) = [base, off]

opcode :: Expr' a -> Text
opcode (Add _ _) = "add"
opcode (Sub _ _) = "sub"
opcode (Mul _ _) = "mul"
opcode (Div _ _) = "div"
opcode (FAdd _ _) = "fadd"
opcode (FSub _ _) = "fsub"
opcode (FMul _ _) = "fmul"
opcode (FDiv _ _) = "fdiv"
opcode (Eq _ _) = "eq"
opcode (Lt _ _) = "lt"
opcode (Gt _ _) = "gt"
opcode (Le _ _) = "le"
opcode (Ge _ _) = "ge"
opcode (FEq _ _) = "feq"
opcode (FLt _ _) = "flt"
opcode (FGt _ _) = "fgt"
opcode (FLe _ _) = "fle"
opcode (FGe _ _) = "fge"
opcode (Not _) = "not"
opcode (And _ _) = "and"
opcode (Or _ _) = "or"
opcode (Call _ _) = "call"
opcode (Id _) = "id"
opcode (Const _) = "const"
opcode (Alloc _) = "alloc"
opcode (Load _) = "load"
opcode (PtrAdd _ _) = "ptradd"

isPure :: Expr' a -> Bool
isPure (Call _ _) = False
isPure _ = True
