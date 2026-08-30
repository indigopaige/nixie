module Compile.Nixie.IR where

import qualified Text.Megaparsec.Char as C
import Control.Lens ((&), makeLenses, (^.))
import qualified Text.Megaparsec as P
import Effectful.Reader.Static
import Control.Applicative
import Data.Scientific
import Text.Nixie.Ast
import Text.Nixie
import Data.Void
import Data.List
import Effectful

data Expr
  = ExprCnd Expr Expr Expr
  | ExprLet Expr Expr
  | ExprApp Expr Expr
  | ExprInt Integer
  | ExprDec Double
  | ExprStr String
  | ExprLst [Expr]
  | ExprTup [Expr]
  | ExprUnb Ident
  | ExprBin Bool
  | ExprChr Char
  | ExprAbs Expr
  | ExprVar Int
  deriving ( Show
           , Ord
           , Eq
           )

-- De Bruijn Index building
fromExpression :: Expression -> Expr
fromExpression expr = f expr & runPureEff . runReader @[Ident] []
  where
    f (ExpressionLet pat fst snd) = local (vars pat ++) $ ExprLet <$> f fst <*> f snd
    f (ExpressionCon fst snd thd) = ExprCnd <$> f fst <*> f snd <*> f thd
    f (ExpressionLam pat expr)    = local (vars pat ++) $ ExprAbs <$> f expr
    f (ExpressionApp fst snd)     = ExprApp <$> f fst <*> f snd
    f (ExpressionLst lst)         = ExprLst <$> traverse f lst
    f (ExpressionTup tup)         = ExprTup <$> traverse f tup
    f (ExpressionLit lit)         = pure $ g lit
      where
        g (LiteralChar chr)   = ExprChr chr
        g (LiteralBool bin)   = ExprBin bin

        g (LiteralInteger i)  = ExprInt i
        g (LiteralDecimal d)  = ExprDec d

        g (LiteralString str) = ExprStr str
    f (ExpressionVar var)         = g . elemIndex var <$> ask
      where
        g (Just x) = ExprVar (fromIntegral x)
        g Nothing  = ExprUnb var

newtype TyVar = TV Ident deriving (Eq, Ord, Show)

data Ty
  = TyCon Ident [Ty]
  | TyVar TyVar
  deriving ( Show
           , Ord
           , Eq
           )

infixr 9 :->

pattern TyLst a = TyCon (Ident "lst") [a]
pattern TyTup a = TyCon (Ident "tup") a
pattern TyInt   = TyCon (Ident "int") []
pattern TyDec   = TyCon (Ident "dec") []
pattern TyStr   = TyCon (Ident "str") []
pattern TyChr   = TyCon (Ident "chr") []
pattern TyBin   = TyCon (Ident "bin") []
pattern a :-> b = TyCon (Ident "->") [a, b]

-- Builtin primitive types are assigned from identifiers here, they are not reserved names, but resesrved types
fromType :: Type -> Ty
fromType (TypeArr left right) = (fromType left) :-> (fromType right)
fromType (TypeCon ident args) = TyCon ident (map fromType args)

data FnSig = FnSig
  { _fnSigName :: Ident
  , _fnSigTy   :: Ty
  }
  deriving ( Show
           , Ord
           , Eq
           )

data FnFun = FnFun
  { _fnFunName :: Ident
  , _fnFunExpr :: Expr
  }
  deriving ( Show
           , Ord
           , Eq
           )

data FnDef = FnDef
  { _fnDefSig :: Maybe FnSig
  , _fnDefFun :: FnFun
  }
  deriving ( Show
           , Ord
           , Eq
           )

fromFunctionDefinition :: FunctionDefinition -> FnDef
fromFunctionDefinition (FunctionDefinition s f) = FnDef sf ff
  where
    sf = (\s -> FnSig (functionSignatureName s) $ fromType (functionSignatureType s)) <$> s
    ff = FnFun (functionBodyName f) $ fromExpression (functionBodyExpr f)

makeLenses ''FnDef
makeLenses ''FnSig
makeLenses ''FnFun

expr :: Nixie Expr
expr = fromExpression <$> expression

fnDef :: Nixie FnDef
fnDef = fromFunctionDefinition <$> functionDefinition

items :: Nixie [FnDef]
items = many $ do { fnDef <* C.space }

data ProgramResult
  = ProgramResultItems FnDef [FnDef]
  | ProgramResultDupMain
  | ProgramResultNoMain
  deriving Show

program :: Nixie ProgramResult
program = f <$> items
  where
    f x  = case partition g x of
             ((_:_:xs), _) -> ProgramResultDupMain
             ([], _)       -> ProgramResultNoMain
             ([x], xs)     -> ProgramResultItems x xs

    g fn = fn^.fnDefFun.fnFunName == (Ident "main")

-- Expr functions for working with debruijn indicies
shift :: Int -> Int -> Expr -> Expr
shift d c (ExprVar x)
  | x >= c                = ExprVar (x + d)
  | otherwise             = ExprVar x

shift d c (ExprAbs x)     = ExprAbs $ shift d (c + 1) x
shift d c (ExprLet x y)   = ExprLet (shift d c x) (shift d (c + 1) y)

shift d c (ExprApp x y)   = ExprApp (shift d c x) (shift d c y)

shift d c (ExprCnd x y z) = ExprCnd
                            (shift d c x)
                            (shift d c y)
                            (shift d c z)

shift d c x               = x

