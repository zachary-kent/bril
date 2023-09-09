module Bril.Syntax.Instr
  ( Instr,
    Instr' (..),
    uses,
    def,
    destType,
    isTerminator,
    opcode,
    isPure,
  )
where

import Bril.Syntax.Expr (Expr (..))
import Bril.Syntax.Expr qualified as Expr
import Bril.Syntax.Literal (Literal)
import Bril.Syntax.Type (Type)
import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (catMaybes, listToMaybe, maybeToList)
import Data.Text (Text)

type Operand = Text

type Label = Text

data Instr' a
  = Assign Text Type (Expr a)
  | Jmp Label
  | Br a Label Label
  | CallEff Text [a]
  | Ret (Maybe a)
  | Print [a]
  | Nop
  | Free a
  | Store a a
  | Speculate
  | Commit
  | Guard a Label
  deriving (Show, Functor, Foldable, Traversable)

type Instr = Instr' Operand

uses :: Instr' a -> [a]
uses (Assign _ _ e) = Expr.uses e
uses (Jmp _) = []
uses (Br cond _ _) = [cond]
uses (CallEff _ args) = args
uses (Ret arg) = maybeToList arg
uses (Print args) = args
uses Nop = []
uses (Free ptr) = [ptr]
uses (Store ptr val) = [ptr, val]
uses Speculate = []
uses Commit = []
uses (Guard cond _) = [cond]

def :: Instr' a -> Maybe Operand
def (Assign x _ _) = pure x
def _ = Nothing

destType :: Instr' a -> Maybe Type
destType (Assign _ t _) = pure t
destType _ = Nothing

labels :: Instr' a -> [Text]
labels (Jmp l) = [l]
labels (Br _ t f) = [t, f]
labels _ = []

funcs :: Instr' a -> [Text]
funcs (CallEff func _) = [func]
funcs (Assign _ _ (Call func _)) = [func]
funcs _ = []

isTerminator :: Instr' a -> Bool
isTerminator (Ret _) = True
isTerminator (Jmp _) = True
isTerminator (Br {}) = True
isTerminator (Guard _ _) = True
isTerminator _ = False

parseUnary :: (Text -> Expr Operand) -> Object -> Parser Instr
parseUnary unop obj = do
  (dest, ty) <- parseDest obj
  [x] <- obj .: "args"
  pure $ Assign dest ty $ unop x

parseDest :: Object -> Parser (Text, Type)
parseDest obj =
  (,) <$> obj .: "dest" <*> obj .: "type"

parseBinary :: (Text -> Text -> Expr Operand) -> Object -> Parser Instr
parseBinary binop obj = do
  (dest, ty) <- parseDest obj
  [x, y] <- obj .: "args"
  pure $ Assign dest ty $ binop x y

parseJmp :: Object -> Parser Instr
parseJmp obj = do
  [l] <- obj .: "labels"
  pure $ Jmp l

parseBr :: Object -> Parser Instr
parseBr obj = do
  [cond] <- obj .: "args"
  [t, f] <- obj .: "labels"
  pure $ Br cond t f

parseCall :: Object -> Parser Instr
parseCall obj = do
  args <- obj .:? "args" .!= []
  [func] <- obj .: "funcs"
  let parseCallValue = do
        (dest, t) <- parseDest obj
        pure $ Assign dest t $ Call func args
  parseCallValue <|> pure (CallEff func args)

parseRet :: Object -> Parser Instr
parseRet obj = do
  obj .:? "args" >>= \case
    Nothing -> pure $ Ret Nothing
    Just (_ : _ : _) -> parseFail "Too many return values"
    Just args -> pure $ Ret $ listToMaybe args

parsePrint :: Object -> Parser Instr
parsePrint obj = Print <$> obj .:? "args" .!= []

parseFree :: Object -> Parser Instr
parseFree obj = do
  [x] <- obj .: "args"
  pure $ Free x

parseStore :: Object -> Parser Instr
parseStore obj = do
  [ptr, val] <- obj .: "args"
  pure $ Store ptr val

parseGuard :: Object -> Parser Instr
parseGuard obj = do
  [cond] <- obj .: "args"
  [onFail] <- obj .: "labels"
  pure $ Guard cond onFail

parseConst :: Object -> Parser Instr
parseConst obj = do
  (dest, ty) <- parseDest obj
  val <- obj .: "value"
  pure $ Assign dest ty (Const val)

instance FromJSON Instr where
  parseJSON =
    withObject "Instr" \obj ->
      obj .: "op" >>= withText "opcode" \case
        "add" -> parseBinary Add obj
        "sub" -> parseBinary Sub obj
        "mul" -> parseBinary Mul obj
        "div" -> parseBinary Div obj
        "fadd" -> parseBinary FAdd obj
        "fsub" -> parseBinary FSub obj
        "fmul" -> parseBinary FMul obj
        "fdiv" -> parseBinary FDiv obj
        "eq" -> parseBinary Eq obj
        "lt" -> parseBinary Lt obj
        "gt" -> parseBinary Gt obj
        "le" -> parseBinary Le obj
        "ge" -> parseBinary Ge obj
        "feq" -> parseBinary FEq obj
        "flt" -> parseBinary FLt obj
        "fgt" -> parseBinary FGt obj
        "fle" -> parseBinary FLe obj
        "fge" -> parseBinary FGe obj
        "not" -> parseUnary Not obj
        "and" -> parseBinary And obj
        "or" -> parseBinary Or obj
        "jmp" -> parseJmp obj
        "br" -> parseBr obj
        "call" -> parseCall obj
        "ret" -> parseRet obj
        "id" -> parseUnary Id obj
        "const" -> parseConst obj
        "print" -> parsePrint obj
        "nop" -> pure Nop
        "free" -> parseFree obj
        "store" -> parseStore obj
        "alloc" -> parseUnary Alloc obj
        "load" -> parseUnary Load obj
        "ptradd" -> parseBinary PtrAdd obj
        "speculate" -> pure Speculate
        "commit" -> pure Commit
        "guard" -> parseGuard obj
        _ -> parseFail "Unknown opcode"

opcode :: Instr -> Text
opcode (Assign _ _ e) = Expr.opcode e
opcode (Jmp _) = "jmp"
opcode (Br {}) = "br"
opcode (CallEff _ _) = "call"
opcode (Ret _) = "ret"
opcode (Print _) = "print"
opcode Nop = "nop"
opcode (Free _) = "free"
opcode (Store _ _) = "store"
opcode Speculate = "speculate"
opcode Commit = "commit"
opcode (Guard _ _) = "guard"

value :: Instr -> Maybe Literal
value (Assign _ _ (Const lit)) = pure lit
value _ = Nothing

isPure :: Instr -> Bool
isPure Nop = True
isPure (Assign _ _ e) = Expr.isPure e
isPure _ = False

instance ToJSON Instr where
  toJSON instr =
    object $
      optionalFields
        ++ [ "op" .= opcode instr,
             "labels" .= labels instr,
             "funcs" .= funcs instr,
             "args" .= uses instr
           ]
    where
      optionalFields =
        catMaybes
          [ ("dest" .=) <$> def instr,
            ("type" .=) <$> destType instr,
            ("value" .=) <$> value instr
          ]
