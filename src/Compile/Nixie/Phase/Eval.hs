module Compile.Nixie.Phase.Eval where

import qualified Text.Megaparsec as P
import Compile.Nixie.Phase.Check
import qualified Data.Map as Map
import Effectful.Reader.Static
import Effectful.Error.Static
import Compile.Nixie.IR
import Text.Nixie.Ast
import Data.Map (Map)
import Data.Foldable
import Control.Monad
import Control.Lens
import Text.Nixie
import Data.Void
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

type Env es =
  ( Reader (Map Ident Expr) :> es
  )

eval :: Env es => Expr -> Eff es Expr
eval (ExprApp u@(ExprUnb (Ident i)) a) = do
  a' <- eval a
  g i a'
  where
    g i a = do
      m <- ask
      case Map.lookup (Ident i) m of
        Just x  -> eval (ExprApp x a)
        Nothing -> pure $ ExprApp u a

eval (ExprApp (ExprApp (u@(ExprUnb (Ident i))) f) a)
  = do
      f' <- eval f
      a' <- eval a
      g i f' a'
    where
      g "equal" x y                        = pure $ ExprBin (x == y)
      g "add_wrd" (ExprWrd i) (ExprWrd i') = pure $ ExprWrd (i + i')
      g "sub_wrd" (ExprWrd i) (ExprWrd i') = pure $ ExprWrd (i - i')
      g "mul_wrd" (ExprWrd i) (ExprWrd i') = pure $ ExprWrd (i * i')
      g "div_wrd" (ExprWrd i) (ExprWrd i') = pure $ ExprWrd (div i i')
      g "add_int" (ExprInt i) (ExprInt i') = pure $ ExprInt (i + i')
      g "sub_int" (ExprInt i) (ExprInt i') = pure $ ExprInt (i - i')
      g "mul_int" (ExprInt i) (ExprInt i') = pure $ ExprInt (i * i')
      g "div_int" (ExprInt i) (ExprInt i') = pure $ ExprInt (div i i')
      g "add_dec" (ExprDec i) (ExprDec i') = pure $ ExprDec (i + i')
      g "sub_dec" (ExprDec i) (ExprDec i') = pure $ ExprDec (i - i')
      g "mul_dec" (ExprDec i) (ExprDec i') = pure $ ExprDec (i * i')
      g "div_dec" (ExprDec i) (ExprDec i') = pure $ ExprDec (i / i')
      g n f a                              = do
        m <- ask
        case Map.lookup (Ident n) m of
          Just x  -> eval (ExprApp (ExprApp x f) a)
          Nothing -> pure $ ExprApp (ExprApp u f) a

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

fnEnv :: [Item] -> Map Ident Expr
fnEnv = Map.fromList . map f . filter isFn
  where
    isFn (ItemFnDef _) = True
    isFn _             = False
    f (ItemFnDef fn)   = (fn ^. fnDefFun.fnFunName, fn ^.fnDefFun.fnFunExpr)

evalItems :: Map Ident Expr -> FnDef -> [Item] -> Expr
evalItems env main items = eval (main^.fnDefFun.fnFunExpr) & runPureEff . runReader env

data EvalError
  = EvalErrorParseError (P.ParseErrorBundle String Void)
  | EvalErrorTypeCheck String
  | EvalErrorMain
  deriving Show

type EvalItems es =
  ( Error EvalError :> es
  , IOE :> es
  )

checkItem :: Error EvalError :> es => Map Ident Expr -> Item -> Eff es ()
checkItem env (ItemFnDef fn) = do
  c <- runError $ void $ inferFnTy env fn
  case c of
    Left (_, e) -> throwError (EvalErrorTypeCheck e)
    Right _     -> pure()
checkItem _ _                = pure ()

evalItemsInFn :: String -> IO (Either (CallStack, EvalError) Expr)
evalItemsInFn e = runEff . runError $ do
  a <- liftIO $ parseFile prog e
  case a of
    Right (Just (x, i@(ItemFnDef f))) -> do
      let env = fnEnv x
      traverse_ (checkItem env) x
      checkItem env i
      pure $ evalItems env f x
    Left  x                       -> throwError (EvalErrorParseError x)
    _                             -> throwError EvalErrorMain
