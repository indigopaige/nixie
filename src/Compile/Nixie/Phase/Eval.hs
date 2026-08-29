module Compile.Nixie.Phase.Eval where

import Compile.Nixie.Phase.Check
import Effectful.Error.Static
import Compile.Nixie.IR
import Text.Nixie.Ast
import Effectful

type Eval es =
  ( Error String :> es
  )

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

red :: Expr -> Expr
red (ExprApp (ExprAbs b) a) = red $ shift (-1) 0 $ subst 0 (shift 1 0 a) b

red (ExprApp f x)           = red $ ExprApp (red f) (red x)
red (ExprAbs b)             = ExprAbs (red b)
red (ExprCnd c t e)         = ExprCnd (red c) (red t) (red e)
red (ExprLet v b)           = ExprLet (red v) (red b)
red (ExprLst xs)            = ExprLst (map red xs)
red (ExprTup xs)            = ExprTup (map red xs)
red e                       = e
