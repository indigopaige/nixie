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
  | ExpressionLit Literal
  | ExpressionVar Ident
  deriving ( Show
           , Ord
           , Eq
           )

data Pattern
  = PatternVar Ident
    deriving ( Show
             , Ord
             , Eq
             )

vars :: Pattern -> [Ident]
vars (PatternVar x)   = [x]

data Literal
  = LiteralInteger Integer
  | LiteralDecimal Double
  | LiteralBool Bool
  deriving ( Show
           , Ord
           , Eq
           )

data FunctionDefinition = FunctionDefinition
  { functinonDefinitionFunctionSignature :: Maybe FunctionSignature
  , functinonDefinitionFuntionBody      :: FunctionBody
  }
  deriving ( Show
           , Ord
           , Eq
           )

data FunctionSignature = FunctionSignature
  { functionSignatureName :: Ident
  , functionSignatureType :: Type
  }
  deriving ( Show
           , Ord
           , Eq
           )

data FunctionBody = Function
  { functionBodyName :: Ident
  , functionBodyExpr :: Expression
  }
  deriving ( Show
           , Ord
           , Eq
           )

data Type
  = TypeArr Type Type
  | TypeCon Ident [Type]
  deriving ( Show
           , Ord
           , Eq
           )
