module Compile.Nixie.Phase.Exec where

import qualified Text.Megaparsec as P
import Compile.Nixie.Phase.Check
import qualified Data.Map as Map
import Compile.Nixie.Phase.Eval
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

import Debug.Trace

data ExecError
  = ExecErrorNoMain
  | ExecErrorDupMain
  | ExecErrorTypeCheck String
  | ExecErrorParse (P.ParseErrorBundle String Void)
  deriving Show

type Exec es =
  ( Error ExecError :> es
  , ParseFile es
  , InferTy es
  , Env' es
  )

itemsEnv :: [FnDef] -> Map Ident Expr
itemsEnv = Map.fromList . map f
  where
    f fn = ( fn^.fnDefFun.fnFunName
           , fn^.fnDefFun.fnFunExpr
           )

exec :: Exec es => String -> Eff es Expr
exec fileName = parseFile program fileName >>= f
  where
    f ProgramResultNoMain           = throwError ExecErrorNoMain
    f ProgramResultDupMain          = throwError ExecErrorDupMain
    f (ProgramResultItems mf items) = do
      local @(Map Ident Expr) (Map.union (itemsEnv items)) $ do
        traverse_ inferTy items
        inferTy mf
        eval (mf^.fnDefFun.fnFunExpr)

runExec_
  :: Map Ident (Expr -> Expr -> Expr)
  -> Map Ident Expr
  -> Map Ident Scheme
  -> Eff '[ Error ExecError
          , Error (P.ParseErrorBundle String Void)
          , Error String
          , Reader (Map Ident Scheme)
          , Reader (Map Ident Expr)
          , Reader (Map Ident (Expr -> Expr -> Expr))
          , IOE
          ] a
  -> IO (Either String
           (Either (P.ParseErrorBundle String Void)
             (Either ExecError a)))
runExec_ ops env tyEnv act
  = runEff
  . runReader ops
  . runReader env
  . runReader tyEnv
  . runErrorNoCallStack @String
  . runErrorNoCallStack @(P.ParseErrorBundle String Void)
  . runErrorNoCallStack @ExecError
  $ act

type Builtins = Map Ident (Expr -> Expr -> Expr)
type UserDefs = Map Ident Expr
type PrimTys  = Map Ident Scheme

runExec
  :: Builtins
  -> UserDefs
  -> PrimTys
  -> Eff '[ Error ExecError
          , Error (P.ParseErrorBundle String Void)
          , Error String
          , Reader (Map Ident Scheme)
          , Reader (Map Ident Expr)
          , Reader (Map Ident (Expr -> Expr -> Expr))
          , IOE
          ] a
  -> IO (Either ExecError a)

runExec b u p m = f <$> runExec_ b u p m
  where
    f (Left str)                = Left (ExecErrorTypeCheck str)
    f (Right (Left pr))         = Left (ExecErrorParse pr)
    f (Right (Right (Left e)))  = Left e
    f (Right (Right (Right a))) = pure a

execWithDefault :: String -> IO (Either ExecError Expr)
execWithDefault = runExec b u p . exec
  where
    b = builtinFun
    u = mempty
    p = builtinTys
