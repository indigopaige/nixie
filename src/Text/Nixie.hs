module Text.Nixie where

import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char
import Text.Megaparsec
import Text.Nixie.Ast
import Control.Monad
import Debug.Trace
import Data.Maybe
import Data.Void

type Nixie = Parsec Void String

parens, braces, angles, brackets :: Nixie a -> Nixie a
parens    = between (L.symbol space "(" <* space) (space *> L.symbol space ")")
braces    = between (L.symbol space "{" <* space) (space *> L.symbol space "}")
angles    = between (L.symbol space "<" <* space) (space *> L.symbol space ">")
brackets  = between (L.symbol space "[" <* space) (space *> L.symbol space "]")

sepBySpace, sepBySpace1 :: Nixie a -> Nixie [a]

sepBySpace1 = (`sepBy1` space1)
sepBySpace  = (`sepBy` hspace)

sepByComma :: Nixie a -> Nixie [a]
sepByComma = (`sepBy` sym ",")

sepByBar :: Nixie a -> Nixie [a]
sepByBar = (`sepBy` sym "|")

sym :: String -> Nixie String
sym = L.symbol space

expression :: Nixie Expression
expression = msum [ con
                  , lht
                  , try lam
                  , try app
                  , parens expression
                  , tup
                  , lst
                  , var
                  , lit
                  ]
  where
    lam = ExpressionLam <$> patterns <* sym "." <*> expression

    lht = do
      sym "let"
      pat <- patterns
      space
      sym "="
      exp <- expression
      space
      sym "in"
      exp' <- expression
      pure $ ExpressionLet pat exp exp'

    con = do
      sym "if"
      eval <- expression
      space
      sym "then"
      runt <- expression
      space
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
patterns = msum [ try rcd
               , try unt
               , try bnd
               , lit
               , var
               , tup
               , lst
               ]
  where
    rcd = PatternRec <$> ident <* space <*> braces (sepByComma patterns)
    bnd = PatternBnd <$> ident <* L.symbol space "@" <*> patterns
    unt = PatternUnt <$> ident <* space <*> sepBySpace1 patterns
    lst = PatternLst <$> brackets (sepByComma patterns)
    tup = PatternTup <$> parens (sepByComma patterns)
    lit = PatternLit <$> literal
    var = PatternVar <$> ident

literal :: Nixie Literal
literal = msum [ number
               , bool
               ]
  where
    number = LiteralNumber <$> L.scientific <?> "literal number"
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
    f = letterChar >> many alphaNumChar
    g = char '_' >> some alphaNumChar
    h = match $ msum [f, g]

constructor :: Nixie Constructor
constructor = msum [ try rcd
                   , unt
                   ]
  where
    rcd = ConstructorRecord <$> ident <* space <*> braces (sepByComma f)
      where
        f = (,) <$> ident <* (space *> sym ":") <*> ident

    unt = ConstructorUnit <$> ident <* space <*> sepBySpace ident

typ :: Nixie Typ
typ = do
  sym "type"
  name <- ident
  hspace
  sym "="
  cons <- sepByBar constructor
  pure $ Typ name cons

-- name : Type
-- name = expr

signature :: Nixie Signature
signature = do
  name <- ident
  space
  sym ":"
  Signature name <$> ty
  where
    ty = try arrow <|> Fst <$> ident
      where
        arrow = do
          left <- Fst <$> ident
          space
          sym "->"
          Arr left <$> ty

function :: Nixie Function
function = do
  name <- ident
  space
  sym "="
  expr <- expression
  pure $ Function name expr

definition :: Nixie Definition
definition = do
  s <- optional (try signature)
  if isJust s
  then do
       space
       f <- function
       pure $ Definition s f
  else do
       f <- function
       pure $ Definition s f

parseFile :: Nixie a -> String -> IO (Either (ParseErrorBundle String Void ) a)
parseFile m file = parse m file <$> readFile file
