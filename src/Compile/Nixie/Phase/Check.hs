module Compile.Nixie.Phase.Check where

import Text.Nixie (parseFile)
import Compile.Nixie.IR
import Text.Nixie.Ast
import Data.Foldable

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Bifunctor
import Control.Monad
import GHC.Stack

import Effectful.Writer.Static.Local
import Effectful.State.Static.Local
import Effectful.Reader.Static
import Effectful.Error.Static
import Effectful

import qualified Text.Megaparsec as P
import Data.Void

import Control.Lens ((&), (^.))

type Constraints = [Constraint]

data Constraint  = Constraint Ty Ty
  deriving Show

type Context     = [Scheme]
data Scheme      = Forall (Set.Set TyVar) Ty
type Count       = Int

data Env = Env (Map Ident Expr) (Map Ident Ty)

type Infer es =
  ( Reader (Map Ident Scheme) :> es
  , Reader (Map Ident Expr) :> es
  , Writer Constraints :> es
  , Reader Context :> es
  , Error String :> es
  , State Count :> es
  )

constrain :: Infer es => Ty -> Ty -> Eff es ()
constrain x y = tell (Constraint x y:[])

fresh :: Infer es => Eff es Ty
fresh = do
  count <- get @Count
  modify @Count (+ 1)
  return . TyVar . TV $ Ident (show count)


type Subst = Map.Map TyVar Ty

compose :: Subst -> Subst -> Subst
compose a b = Map.map (apply a) (b `Map.union` a)

class Substitutable a where
  apply :: Subst -> a -> a
  tvs   :: a -> Set.Set TyVar

instance Substitutable Ty where
  tvs (TyVar tv)   = Set.singleton tv
  tvs (TyCon _ ts) = foldr (Set.union . tvs) Set.empty ts

  apply s t@(TyVar tv) = Map.findWithDefault t tv s
  apply s (TyCon c ts) = TyCon c $ map (apply s) ts

instance Substitutable Scheme where
  tvs (Forall vs t) = tvs t `Set.difference` vs
  apply s (Forall vs t) = Forall vs $ apply (foldr Map.delete s vs) t

instance Substitutable Constraint where
  tvs (Constraint t1 t2) = tvs t1 `Set.union` tvs t2
  apply s (Constraint t1 t2) = Constraint (apply s t1) (apply s t2)

instance Substitutable a => Substitutable [a] where
  tvs l = foldr (Set.union . tvs) Set.empty l
  apply s = map (apply s)

generalize :: Context -> Ty -> Scheme
generalize ctx t =
  Forall (tvs t `Set.difference` tvs ctx) t

instantiate :: Infer es => Scheme -> Eff es Ty
instantiate (Forall vs t) = do
  let vars = Set.toList vs
  ftvs <- traverse (const fresh) vars
  let subst = Map.fromList (zip vars ftvs)
  pure $ apply subst t

checkFnDef :: Infer es => FnDef -> Eff es ()
checkFnDef fn = do
  act <- infer $ fn^.fnDefFun.fnFunExpr

  case fn^.fnDefSig of
    Nothing -> pure ()
    Just x  -> constrain act (x^.fnSigTy)

class Inferable a where
  infer :: Infer es => a -> Eff es Ty

instance Inferable FnDef where
  infer (FnDef (Just sig) body) = do
    a <- fresh
    constrain a (sig^.fnSigTy)
    b <- infer (body^.fnFunExpr)
    constrain b a
    pure b

  infer (FnDef _ body) = infer (body^.fnFunExpr)

instance Inferable Expr where
  infer = \case
    ExprUnb (i@(Ident x)) -> do
      m  <- ask @(Map Ident Scheme)
      m' <- ask @(Map Ident Expr)
      case Map.lookup i m of
        Just x  -> instantiate x
        Nothing -> case Map.lookup i m' of
          Just x  -> infer x
          Nothing -> throwError $ "unbound variable " <> x

    -- primitive function types
    ExprInt _         -> pure TyInt
    ExprDec _         -> pure TyDec
    ExprStr _         -> pure TyStr
    ExprChr _         -> pure TyChr
    ExprBin _         -> pure TyBin

    ExprTup s         -> do
      sts <- traverse infer s
      pure $ TyTup sts

    ExprLst s         -> do
      sts <- traverse infer s
      f   <- fresh

      traverse_ (constrain f) sts
      pure $ TyLst f

    ExprVar ix        -> do
      ctx <- ask @Context
      case drop (fromIntegral ix) ctx of
        (t : _) -> instantiate t
        []      -> throwError "variable not defined" -- should never happen if created via expr

    ExprCnd c a b     -> do
      ct <- infer c
      at <- infer a
      bt <- infer b
      constrain ct TyBin
      constrain at bt
      pure at

    ExprAbs e         -> do
      pt <- fresh
      let ps = Forall Set.empty pt
      et <- local (ps :) (infer e)
      pure $ pt :-> et

    ExprApp f a       -> do
      ft <- infer f
      at <- infer a
      rt <- fresh
      constrain ft (at :-> rt)
      pure rt

    ExprLet e b       -> do
      (et, cs) <- listen (infer e)
      subst    <- runSolve cs
      let et' = apply subst et
      ctx      <- ask
      let ctx' = apply subst ctx
      let es   = generalize ctx' et'
      local (const (es : ctx')) (infer b)
 
type Solve es =
  (  Error String :> es
  )

unify :: Solve es => Ty -> Ty -> Eff es Subst
unify a b | a == b     = pure $ Map.empty
unify (TyVar v) t      = bind v t
unify t (TyVar v)      = bind v t

unify a@(TyCon n ts) b@(TyCon n' ts')
  | n /= n'            = throwError $ "type mismatch " <> show a <> " and " <> show b
  | otherwise          = unifyMany ts ts'
  where
    unifyMany [] []               = pure $ Map.empty
    unifyMany (t : ts) (t' : ts') = do
      s  <- unify t t'
      s' <- unifyMany (apply s ts) (apply s ts')
      pure $ s' `compose` s

bind :: Solve es => TyVar -> Ty -> Eff es Subst
bind v t
  -- occurs check
  | v `Set.member` tvs t = throwError $ "infinite type " <> show v <> " ~ " <> show t
  | otherwise            = pure $ Map.singleton v t

solve :: Solve es => Subst -> [Constraint] -> Eff es Subst
solve s []                       = pure s
solve s ((Constraint t t') : cs) = do
  s' <- unify t t'
  solve (s' `compose` s) (apply s' cs)

runSolve :: Error String :> es => [Constraint] -> Eff es Subst
runSolve = solve Map.empty

type InferTy es =
  ( Reader (Map Ident Scheme) :> es
  , Reader (Map Ident Expr) :> es
  , Error String :> es
  )

runInfer :: (Inferable i, InferTy es) => i -> Eff es (Ty, Constraints)
runInfer i = infer i & evalState @Count 0
                . runReader @Context []
                . runWriter @Constraints

inferTy :: (Inferable i, InferTy es) => i -> Eff es Ty
inferTy expr = do
  (t, cs) <- runInfer expr
  s       <- runSolve cs
  pure $ apply s t

mon :: Ty -> Scheme
mon = Forall mempty
