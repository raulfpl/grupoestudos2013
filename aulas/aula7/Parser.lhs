> module Parser where

> import Text.ParserCombinators.Parsec
> import Text.ParserCombinators.Parsec.Expr
> import qualified Text.Parsec.Token as P
> import Text.Parsec.Language (haskellStyle)
> import Data.Functor

> data Exp = Const Int
>          | Add Exp Exp
>          | Mult Exp Exp
>          deriving (Eq, Ord, Show)

> expr :: Parser Exp
> expr = buildExpressionParser table factor <?> "expression"

> table :: [[ Operator Char st Exp ]]
> table = [[ op "*" Mult AssocLeft],
>          [ op "+" Add  AssocLeft]]
>         where
>           op s f assoc = Infix (do {P.symbol lexer s ; return f}) assoc

> factor = try (parens expr)
>          <|> number
>          <?> "simple expression"
  
> number :: Parser Exp
> number = (Const . fromInteger <$> P.natural lexer) <?> "number"

> parens = P.parens lexer

> lexer = P.makeTokenParser haskellStyle
