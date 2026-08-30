module Compile.Nixie.Phase.Eval where

import qualified Text.Megaparsec as P
import Compile.Nixie.Phase.Check
import qualified Data.Map as Map
import Effectful.Reader.Static
import Effectful.Error.Static
import Compile.Nixie.Env
import Compile.Nixie.IR
import Text.Nixie.Ast
import Data.Map (Map)
import Data.Foldable
import Control.Monad
import Control.Lens
import Text.Nixie
import Data.Void
import Effectful

subst :: Int -> Expr -> Expr -> Expr
subst j s = f 0
  where
    f d e = g e
      where
        g (ExprVar k)
          | k == j + d    = shift d 0 s
          | otherwise     = ExprVar k
        g (ExprAbs b)     = ExprAbs (f (d + 1) b)

        g (ExprApp a x)   = ExprApp (f d a) (f d x)

        g (ExprCnd c t e) = ExprCnd (f d c) (f d t) (f d e)

        g (ExprLet v b)   = ExprLet (f d v) (f (d + 1) b)

        g (ExprLst l)     = ExprLst (map (f d) l)
        g (ExprTup t)     = ExprTup (map (f d) t)

        g x               = x

type Env' es =
  ( Reader (Map Ident (Expr -> Expr -> Expr)) :> es
  , Reader (Map Ident Expr) :> es
  )

eval :: Env' es => Expr -> Eff es Expr
eval (ExprApp (ExprApp (u@(ExprUnb (Ident i))) f) a) = do
  f' <- eval f
  a' <- eval a
  g i f' a'
  where
    g n f a = do
      m  <- ask @(Map Ident Expr)
      m' <- ask @(Map Ident (Expr -> Expr -> Expr))
      case Map.lookup (Ident n) m of
        Just x  -> eval (ExprApp (ExprApp x f) a)
        Nothing -> case Map.lookup (Ident n) m' of
          Just f' -> pure (f' f a)
          Nothing -> pure $ ExprApp (ExprApp u f) a

eval (ExprUnb (Ident i)) = do
  m <- ask
  case Map.lookup (Ident i) m of
    Just x  -> eval x
    Nothing -> pure $ ExprUnb (Ident i)

eval (ExprApp f a)    = do
  f' <- eval f
  case f' of
    ExprAbs b -> eval $ shift (-1) 0 $ subst 0 (shift 1 0 a) b
    f'        -> ExprApp f' <$> eval a

eval (ExprLet v b) = do
  v' <- eval v
  eval $ shift (-1) 0 $ subst 0 (shift 1 0 v') b

eval (ExprAbs b)     = ExprAbs <$> eval b

eval (ExprCnd c t e) = do
  c' <- eval c
  case c' of
    ExprBin True  -> eval t
    ExprBin False -> eval e
    c'            -> pure $ ExprCnd c' t e

eval (ExprLst xs)    = ExprLst <$> mapM eval xs
eval (ExprTup xs)    = ExprTup <$> mapM eval xs
eval e               = pure e
