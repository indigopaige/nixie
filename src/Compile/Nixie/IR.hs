module Compile.Nixie.IR where

import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec as P
import Control.Lens ((&), makeLenses)
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
  | ExprDec Double
  | ExprStr String
  | ExprLst [Expr]
  | ExprTup [Expr]
  | ExprUnb Ident
  | ExprWrd Word
  | ExprVar Word
  | ExprBin Bool
  | ExprChr Char
  | ExprAbs Expr
  | ExprInt Int
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

        g (LiteralNumber num)
          | Left r  <- floatingOrInteger num = ExprDec (realToFrac r)
          | Right r <- floatingOrInteger num = if signum r == -1
                                               then ExprInt (fromIntegral r)
                                               else ExprWrd (fromIntegral r)

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
pattern TyTup a = TyCon (Ident "lst") a
pattern TyInt   = TyCon (Ident "int") []
pattern TyWrd   = TyCon (Ident "wrd") []
pattern TyDec   = TyCon (Ident "dec") []
pattern TyStr   = TyCon (Ident "str") []
pattern TyChr   = TyCon (Ident "chr") []
pattern TyBin   = TyCon (Ident "bin") []
pattern a :-> b = TyCon (Ident "->") [a, b]

-- Builtin primitive types are assigned from identifiers here, they are not reserved names, but resesrved types
fromTypeName :: TypeName -> Ty
fromTypeName (Arr left right)        = (fromTypeName left) :-> (fromTypeName right)
fromTypeName (Fst ident@(Ident(id))) = f id
  where
    f "int" = TyCon ident []
    f "dec" = TyCon ident []
    f "wrd" = TyCon ident []
    f "str" = TyCon ident []
    f "chr" = TyCon ident []
    f "bin" = TyCon ident []
    f _     = TyCon ident []

-- Wrapper for instances, nothing special
newtype Cons = Cons Constructor
  deriving ( Show
           , Ord
           , Eq
           )

fromConstructor :: Constructor -> Cons
fromConstructor = Cons

data TyDef = TyDef
  { _tyName :: Ident
  , _tyCons :: [Cons]
  }
  deriving ( Show
           , Ord
           , Eq
           )

fromTyp :: Typ -> TyDef
fromTyp (Typ n c) = TyDef n (map fromConstructor c)

data FnDef= FnDef
  { _fnName :: Ident
  , _fnExpr :: Expr
  }
  deriving ( Show
           , Ord
           , Eq
           )

fromFunction :: Function -> FnDef
fromFunction (Function n e) = FnDef n (fromExpression e)

makeLenses ''FnDef
makeLenses ''TyDef

data Item
  = ItemFnDef FnDef
  | ItemTyDef TyDef
  deriving ( Show
           , Ord
           , Eq
           )

expr :: Nixie Expr
expr = fromExpression <$> expression

item :: Nixie Item
item = fmap g function <|> fmap f typ
  where
    g = ItemFnDef . fromFunction
    f = ItemTyDef . fromTyp

prog :: Nixie [Item]
prog = many (item <* C.space)

-- Expr functions for working with debruijn indicies
shift :: Word -> Word -> Expr -> Expr
shift d c (ExprVar x)
  | x >= c                = ExprVar (x + d)
  | otherwise             = ExprVar x

shift d c (ExprAbs x)     = ExprAbs $ shift d (c + 1) x
shift d c (ExprLet x y)   = ExprLet (s x) (s y)
  where
    s = shift d (c + 1)

shift d c (ExprApp x y)   = ExprApp (shift d c x) (shift d c y)

shift d c (ExprCnd x y z) = ExprCnd
                            (shift d c x)
                            (shift d c y)
                            (shift d c z)

shift d c x               = x
