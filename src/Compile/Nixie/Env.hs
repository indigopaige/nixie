module Compile.Nixie.Env where

import qualified Data.Set as Set
import Compile.Nixie.Phase.Check
import qualified Data.Map as Map
import Compile.Nixie.IR
import Data.Map (Map)
import Text.Nixie.Ast

equal :: Scheme
equal = Forall (Set.singleton a) (TyVar a :-> TyVar a :-> TyBin)
  where
    a = TV (Ident "a")

builtinFun :: Map Ident (Expr -> Expr -> Expr)
builtinFun = Map.fromList
  [ entry "equal"   (\_ f a -> ExprBin (f == a))

    -- int arithmetic
  , entry "add_int" (numOp exprInt (+))
  , entry "sub_int" (numOp exprInt (-))
  , entry "mul_int" (numOp exprInt (*))
  , entry "div_int" (numOp exprInt div)

    -- dec arithmetic
  , entry "add_dec" (numOp exprDec (+))
  , entry "sub_dec" (numOp exprDec (-))
  , entry "mul_dec" (numOp exprDec (*))
  , entry "div_dec" (numOp exprDec (/))
  ]
  where
    entry name f = let ident = Ident name in (ident, f ident)
    stuck ident f a = ExprApp (ExprApp (ExprUnb ident) f) a
    exprInt = (\case ExprInt x -> Just x; _ -> Nothing, ExprInt)
    exprDec = (\case ExprDec x -> Just x; _ -> Nothing, ExprDec)
    numOp (match, build) op ident f a =
      case (match f, match a) of
        (Just x, Just y) -> build (op x y)
        _                -> stuck ident f a

builtinTys :: Map Ident Scheme
builtinTys = Map.fromList
  [ (Ident "equal", equal)
    -- int arithmetic
  , (Ident "add_int", mon i3)
  , (Ident "sub_int", mon i3)
  , (Ident "mul_int", mon i3)
  , (Ident "div_int", mon i3)
    -- dec arithmetic
  , (Ident "add_dec", mon d3)
  , (Ident "sub_dec", mon d3)
  , (Ident "mul_dec", mon d3)
  , (Ident "div_dec", mon d3)
  ]
  where
    i3 = TyInt :-> TyInt :-> TyInt
    d3 = TyDec :-> TyDec :-> TyDec
x
