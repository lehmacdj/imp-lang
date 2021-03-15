module Parser
  ( parseAExp,
    parseBExp,
    parseCommand,
    parseProgram,
  )
where

import AST
import Control.Applicative hiding ((<|>))
import Control.Arrow
import Data.Functor.Identity (Identity)
import System.FilePath (dropExtension, takeExtension)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

type Parser = Parsec String ()

tokenParser :: P.TokenParser u
tokenParser =
  P.makeTokenParser
    emptyDef
      { P.commentLine = "#",
        P.identStart = letter,
        P.identLetter = alphaNum,
        P.opStart = oneOf "+*-:><=/&|~",
        P.opLetter = oneOf "+*-:><=/&|~",
        P.reservedOpNames =
          [ "+",
            "-",
            "*",
            "==",
            ">=",
            "<=",
            ">",
            "<",
            "/=",
            "~",
            "&&",
            "||",
            ":="
          ],
        P.reservedNames =
          [ "while",
            "if",
            "then",
            "else",
            "do",
            "skip",
            "true",
            "false",
            "not"
          ],
        P.caseSensitive = True
      }

P.TokenParser
  { P.reservedOp = reservedOp,
    P.reserved = reserved,
    P.parens = parens,
    P.semiSep1 = semiSep1,
    P.identifier = identifier,
    P.whiteSpace = whiteSpace,
    P.integer = integer
  } = tokenParser

binop :: String -> (a -> a -> a) -> Assoc -> Operator String u Identity a
binop name fn = Infix ((reservedOp name <|> reserved name) *> pure fn)

prefix :: String -> (a -> a) -> Operator String u Identity a
prefix name fn = Prefix ((reservedOp name <|> reserved name) *> pure fn)

aexp :: Parser AExp
aexp =
  buildExpressionParser
    [ [binop "*" Mult AssocLeft],
      [binop "+" Add AssocLeft, binop "-" Sub AssocLeft]
    ]
    aterm

aterm :: Parser AExp
aterm =
  parens aexp
    <|> fmap N integer
    <|> fmap Var identifier

bexp :: Parser BExp
bexp =
  buildExpressionParser
    [ [prefix "not" Not],
      [binop "&&" And AssocLeft],
      [binop "||" Or AssocLeft]
    ]
    bterm

bterm :: Parser BExp
bterm =
  parens bexp
    <|> true
    <|> false
    <|> aexp <**> comparison <*> aexp

true :: Parser BExp
true = reserved "true" *> pure (B True)

false :: Parser BExp
false = reserved "false" *> pure (B False)

cmpop :: String -> (AExp -> AExp -> BExp) -> Parser (AExp -> AExp -> BExp)
cmpop name fn = reservedOp name *> pure fn

comparison :: Parser (AExp -> AExp -> BExp)
comparison =
  cmpop "==" Eq
    <|> cmpop "/=" NEq
    <|> try (cmpop ">=" GEq)
    <|> cmpop ">" Gt
    <|> try (cmpop "<=" LEq)
    <|> cmpop "<" Lt

commands :: Parser Command
commands = foldl1 (:>) <$> semiSep1 command

command :: Parser Command
command = skip <|> set <|> if' <|> while

skip :: Parser Command
skip = reserved "skip" *> pure Skip

set :: Parser Command
set = identifier <**> (reservedOp ":=" *> pure (:=)) <*> aexp

while :: Parser Command
while = do
  reserved "while"
  b <- bexp
  reserved "do"
  c <- parens commands <|> command
  pure $ While b c

if' :: Parser Command
if' = do
  reserved "if"
  b <- bexp
  reserved "then"
  c1 <- parens commands <|> command
  reserved "else"
  c2 <- parens commands <|> command
  pure $ If b c1 c2

parseAExp :: String -> Either String AExp
parseAExp = left show . parse (whiteSpace *> aexp <* eof) "AExp"

parseBExp :: String -> Either String BExp
parseBExp = left show . parse (whiteSpace *> bexp <* eof) "BExp"

parseCommand :: String -> Either String Command
parseCommand = left show . parse (whiteSpace *> commands <* eof) "imp"

-- | parse a program from a file, returning either a parse error or the
-- resulting command, doesn't attempt to catch IO errors, so throws anything
-- throwable by reading the file
parseProgram :: FilePath -> IO (Either String Command)
parseProgram fp =
  case takeExtension fp of
    ".imp" -> do
      contents <- readFile fp
      pure
        . left show
        . parse (whiteSpace *> commands <* eof) (dropExtension fp)
        $ contents
    extension ->
      pure . Left $
        "error: imp programs should have extension .imp, but has extension: "
          ++ extension
