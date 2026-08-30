module Text.Nixie where

import qualified Text.Megaparsec.Char.Lexer as L
import Effectful.Error.Static
import Text.Megaparsec.Char
import Text.Megaparsec
import Text.Nixie.Ast
import Control.Monad
import Debug.Trace
import Data.Maybe
import Effectful
import Data.Void

type Nixie = Parsec Void String

parens, braces, angles, brackets :: Nixie a -> Nixie a
parens    = between (L.symbol hspace "(" <* hspace) (hspace *> L.symbol hspace ")")
braces    = between (L.symbol hspace "{" <* hspace) (hspace *> L.symbol hspace "}")
angles    = between (L.symbol hspace "<" <* hspace) (hspace *> L.symbol hspace ">")
brackets  = between (L.symbol hspace "[" <* hspace) (hspace *> L.symbol hspace "]")

sym :: String -> Nixie String
sym s = hspace *> L.symbol hspace s

sepBySpace, sepBySpace1 :: Nixie a -> Nixie [a]

sepBySpace1 = (`sepBy1` space1)
sepBySpace  = (`sepBy` hspace)

sepByComma :: Nixie a -> Nixie [a]
sepByComma = (`sepBy` sym ",")

expression :: Nixie Expression
expression = msum [ lit
                  , con
                  , lht
                  , try lam
                  , try app
                  , tup
                  , lst
                  , var
                  , parens expression
                  ]
  where
    lam = ExpressionLam <$> patterns <* sym "." <*> expression

    lht = do
      sym "let"
      pat <- patterns
      sym "="
      exp <- expression
      sym "in"
      exp' <- expression
      pure $ ExpressionLet pat exp exp'

    con = do
      sym "if"
      eval <- expression
      sym "then"
      runt <- expression
      sym "else"
      runf <- expression
      pure $ ExpressionCon eval runt runf

    app = do
      ixp <- atm
      axp <- some $ try $ do { hspace; atm }
      pure $ foldl ExpressionApp ixp axp
      where
        atm = msum [ lit
                   , parens expression
                   , var
                   ]

    lst = ExpressionLst <$> brackets (sepByComma expression)
    tup = ExpressionTup <$> parens (sepByComma expression)
    lit = ExpressionLit <$> literal
    var = ExpressionVar <$> ident

patterns :: Nixie Pattern
patterns = msum [ try bnd
               ,  lit
               ,  var
               ,  tup
               ,  lst
               ]
  where
    bnd = PatternBnd <$> ident <* sym "@" <*> patterns
    lst = PatternLst <$> brackets (sepByComma patterns)
    tup = PatternTup <$> parens (sepByComma patterns)
    lit = PatternLit <$> literal
    var = PatternVar <$> ident

literal :: Nixie Literal
literal = msum [ number
               , bool
               ]
  where
    number = msum
      [ LiteralDecimal <$> try f
      , LiteralInteger <$> i
      ]
      where
        f = L.signed hspace L.float
        i = L.signed hspace L.decimal

    bool   = f <|> t <?> "literal bool"
      where
        f = LiteralBool False <$ string "false"
        t = LiteralBool True  <$ string "true"

ident :: Nixie Ident
ident = do
  i <- Ident . fst <$> (h <?> "ident")
  if isReserved i
  then empty
  else pure i
  where
    f = letterChar >> many (alphaNumChar <|> char '_')
    g = char '_' >> some alphaNumChar
    h = match $ msum [f, g]

functionDefinition :: Nixie FunctionDefinition
functionDefinition = do
  s <- optional (try functionSignature)

  let g = FunctionDefinition s <$> functionBody

  if isJust s
  then space >> g
  else g

functionSignature :: Nixie FunctionSignature
functionSignature = do
  name <- ident
  sym ":"
  FunctionSignature name <$> ty
  where
    ty = msum [ parens ty
              , try arrow
              , con
              ]
      where
        atm   = con <|> parens arrow
        con   = do
          name <- ident
          space
          args <- option [] (brackets $ sepByComma atm)
          pure $ TypeCon name args
        arrow = do
          left <- con
          sym "->"
          TypeArr left <$> ty

functionBody :: Nixie FunctionBody
functionBody = do
  name <- ident
  sym "="
  expr <- expression
  pure $ Function name expr

type ParseFile es =
  ( Error (ParseErrorBundle String Void) :> es
  , IOE :> es
  )

-- First function to call, lifting parse errors into the effect stack
parseFile :: ParseFile es => Nixie a -> String -> Eff es a
parseFile m file = do
  file' <- liftIO $ readFile file
  case parse m file file' of
    Left  x -> throwError x
    Right x -> pure x

runParseFile :: Nixie a -> String -> IO (Either (ParseErrorBundle String Void) a)
runParseFile m s = runEff . runErrorNoCallStack @(ParseErrorBundle String Void) $ parseFile m s
