module Text.Nixie.Ast where

import Data.Scientific

newtype Ident = Ident String
  deriving ( Show
           , Ord
           , Eq
           )

isReserved :: Ident -> Bool
isReserved (Ident "type") = True
isReserved (Ident "else") = True
isReserved (Ident "then") = True
isReserved (Ident "let")  = True
isReserved (Ident "in")   = True
isReserved (Ident "if")   = True
isReserved _              = False

data Expression
  = ExpressionCon Expression Expression Expression
  | ExpressionLet Pattern Expression Expression
  | ExpressionApp Expression Expression
  | ExpressionLam Pattern Expression
  | ExpressionLst [Expression]
  | ExpressionTup [Expression]
  | ExpressionLit Literal
  | ExpressionVar Ident
  deriving ( Show
           , Ord
           , Eq
           )

data Pattern
  = PatternRec Ident [Pattern]
  | PatternUnt Ident [Pattern]
  | PatternBnd Ident Pattern
  | PatternTup [Pattern]
  | PatternLst [Pattern]
  | PatternLit Literal
  | PatternVar Ident
    deriving ( Show
             , Ord
             , Eq
             )

vars :: Pattern -> [Ident]
vars (PatternUnt _ x) = x >>= vars
vars (PatternBnd _ p) = vars p
vars (PatternTup x)   = x >>= vars
vars (PatternLst x)   = x >>= vars
vars (PatternLit _)   = []
vars (PatternVar x)   = [x]

data Literal
  = LiteralNumber Scientific
  | LiteralString String
  | LiteralBool Bool
  | LiteralChar Char
  deriving ( Show
           , Ord
           , Eq
           )
data Cons
  = ConsRecord Ident [(Ident, Ident)]
  | ConsUnit   Ident [Ident]
  deriving ( Show
           , Ord
           , Eq
           )

data Typ = Typ 
  { typName :: Ident
  , typCons :: [Cons]
  }
  deriving ( Show
           , Ord
           , Eq
           )

data Sig
  = SigArr Sig Sig
  | SigTyp Ident
  deriving Show

data Fun = Fun
  { funNam :: Ident
  , funExp :: Expression
  }
  deriving Show

data Def = Def
  { defSig :: Maybe Sig
  , defFun :: Fun
  }
  deriving Show

data Item
  = ItemTyp Typ
  | ItemDef Def
  deriving Show

