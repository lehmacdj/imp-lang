module Parser
    ( readCommand
    ) where

import AST

import Control.Applicative hiding ((<|>))

import Data.Functor.Identity (Identity)

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

type Parser = Parsec String ()

tokenParser :: P.TokenParser u
tokenParser = P.makeTokenParser emptyDef
    { P.commentLine = "#"
    , P.identStart = letter
    , P.identLetter = alphaNum
    , P.opStart = oneOf "+*-:><=/&|~"
    , P.opLetter = oneOf "+*-:><=/&|~"
    , P.reservedOpNames =
        [ "+", "-", "*"
        , "==", ">=", "<=", ">", "<", "/="
        , "~"
        , "&&", "||"
        , ":="
        ]
    , P.reservedNames =
        [ "while"
        , "if"
        , "then"
        , "else"
        , "do"
        , "skip"
        , "true"
        , "false"
        , "not"
        ]
    , P.caseSensitive = True
    }

P.TokenParser
    { P.reservedOp = reservedOp
    , P.reserved = reserved
    , P.parens = parens
    , P.semiSep1 = semiSep1
    , P.identifier = identifier
    , P.whiteSpace = whiteSpace
    , P.integer = integer
    } = tokenParser

binop :: String -> (String -> a -> a -> a) -> Assoc -> Operator String u Identity a
binop name mk = Infix ((reservedOp name <|> reserved name) *> pure (mk name))

prefix :: String -> (String -> a -> a) -> Operator String u Identity a
prefix name mk = Prefix ((reservedOp name <|> reserved name) *> pure (mk name))


aexp :: Parser AExp
aexp = buildExpressionParser
    [ [binop "*" ABinop AssocLeft]
    , [binop "+" ABinop AssocLeft, binop "-" ABinop AssocLeft]
    ]
    aterm

aterm :: Parser AExp
aterm = parens aexp
    <|> fmap N integer
    <|> fmap Var identifier


bexp :: Parser BExp
bexp = buildExpressionParser
    [ [prefix "not" BPrefix]
    , [binop "&&" BBinop AssocLeft]
    , [binop "||" BBinop AssocLeft]
    ]
    bterm

bterm :: Parser BExp
bterm = parens bexp
    <|> true
    <|> false
    <|> aexp <**> comparison <*> aexp

true :: Parser BExp
true = reserved "true" *> pure (B True)

false :: Parser BExp
false = reserved "false" *> pure (B False)

compop :: String -> (Integer -> Integer -> Bool) -> Parser (AExp -> AExp -> BExp)
compop name fn = reservedOp name *> pure (CompOp name)

comparison :: Parser (AExp -> AExp -> BExp)
comparison =
    compop "==" (==)
    <|> compop "/=" (/=)
    <|> try (compop ">=" (>=))
    <|> compop ">" (>)
    <|> try (compop "<=" (<=))
    <|> compop "<" (<)


commands :: Parser Command
commands = foldl1 (:>) <$> semiSep1 command

command :: Parser Command
command = skip <|> set <|> if' <|> while

skip :: Parser Command
skip = reserved "skip" *> pure Skip

set :: Parser Command
set = identifier <**> (reservedOp ":="  *> pure (:=)) <*> aexp

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


readAExp :: String -> Either ParseError AExp
readAExp = parse (whiteSpace *> aexp <* eof) "AExp"

readBExp :: String -> Either ParseError BExp
readBExp = parse (whiteSpace *> bexp <* eof) "BExp"

readCommand :: String -> Either ParseError Command
readCommand = parse (whiteSpace *> commands <* eof) "imp"