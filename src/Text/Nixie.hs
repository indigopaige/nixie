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
                  , try app
                  , parens expression
                  , try lam
                  , tup
                  , lst
                  , var
                  , lit
                  ]
  where
    lam = ExpressionLam <$> pattern <* sym "." <*> expression

    lht = do
      sym "let"
      pat <- pattern
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

pattern :: Nixie Pattern
pattern = msum [ try rcd
               , try unt
               , try bnd
               , lit
               , var
               , tup
               , lst
               ]
  where
    rcd = PatternRec <$> ident <* space <*> braces (sepByComma pattern)
    bnd = PatternBnd <$> ident <* L.symbol space "@" <*> pattern
    unt = PatternUnt <$> ident <* space <*> sepBySpace1 pattern
    lst = PatternLst <$> brackets (sepByComma pattern)
    tup = PatternTup <$> parens (sepByComma pattern)
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

constructor :: Nixie Cons
constructor = msum [ try rcd
                   , unt
                   ]
  where
    rcd = ConsRecord <$> ident <* space <*> braces (sepByComma f)
      where
        f = (,) <$> ident <* (space *> sym ":") <*> ident

    unt = ConsUnit <$> ident <* space <*> sepBySpace ident

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

sig :: Nixie Sig
sig = do
  name <- ident
  space
  sym ":"
  ty
  where
    ty = try arrow <|> SigTyp <$> ident
      where
        arrow = do
          left <- SigTyp <$> ident
          space
          sym "->"
          SigArr left <$> ty

fun :: Nixie Fun
fun = do
  name <- ident
  space
  sym "="
  expr <- expression
  pure $ Fun name expr


def :: Nixie Def
def = do
  s <- optional (try sig)
  if isJust s
  then do
       space
       f <- fun
       pure $ Def s f
  else do
       f <- fun
       pure $ Def s f
  where

item :: Nixie Item
item = (ItemTyp <$> typ) <|> (ItemDef <$> def)

nixie :: Nixie [Item]
nixie = many $ item <* space

parseFile :: Nixie a -> String -> IO (Either (ParseErrorBundle String Void ) a)
parseFile m file = parse m file <$> readFile file
