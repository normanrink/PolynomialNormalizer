
module Parser ( parseExpr ) where


import Text.Parsec as Parsec

import qualified Expr


-- The 'tokenParser' skips trailing whitespace:
tokenParser :: Parsec String u a -> Parsec String u a
tokenParser p = do
  result <- p
  Parsec.spaces
  return result


charParser :: Char -> Parsec String u Char
charParser char = tokenParser $ Parsec.char char


stringParser :: String -> Parsec String u String
stringParser string = tokenParser (do s <- Parsec.string string
                                      Parsec.space
                                      return s)


intParser :: Parsec String () Int
intParser = tokenParser (do digits <- Parsec.many1 Parsec.digit
                            return $ read digits)


idParser :: Parsec String () String
idParser = tokenParser (do start <- Parsec.letter
                           rest  <- Parsec.many Parsec.alphaNum
                           return (start:rest))


opParser :: Expr.Op -> Parsec String () Expr.Op
opParser Expr.Times = charParser '*' >> return Expr.Times
opParser Expr.Minus = charParser '-' >> return Expr.Minus
opParser Expr.Plus  = charParser '+' >> return Expr.Plus


constantParser :: Parsec String () Expr.Expr
constantParser = do i <- intParser
                    return (Expr.Constant i)


parameterParser :: Parsec String () Expr.Expr
parameterParser = do charParser '$'
                     s <- idParser
                     return (Expr.Parameter s)


variableParser :: Parsec String () Expr.Expr
variableParser = do s <- idParser
                    return (Expr.Variable s)


negParser :: Parsec String () Expr.Expr
negParser = do charParser '-'
               e <- exprParser
               return (Expr.Neg e)


atomParser :: Parsec String () Expr.Expr
atomParser = negParser <|> variableParser <|> parameterParser <|> constantParser <|>
             (do -- A parenthesized expression is also an 'atom':
                 charParser '('
                 e <- exprParser
                 charParser ')'
                 return e)


-- Note that the following organisation of parsing functions
-- (with "...RestParser") produces ASTs for products and sums
-- that associate operations to the left:

productRestParser :: Expr.Expr -> Parsec String () Expr.Expr
productRestParser left = (do op <- opParser Expr.Times
                             right <- atomParser
                             productRestParser (Expr.BinOp op left right)) <|>
                         return left

                         
productParser :: Parsec String () Expr.Expr
productParser = do left <- atomParser
                   productRestParser left


sumRestParser :: Expr.Expr -> Parsec String () Expr.Expr
sumRestParser left = (do op <- (opParser Expr.Plus) <|> (opParser Expr.Minus)
                         right <- productParser
                         sumRestParser (Expr.BinOp op left right)) <|>
                     return left
                  
                     
sumParser :: Parsec String () Expr.Expr
sumParser = do left <- productParser
               sumRestParser left
                   

-- A top-level expression is a sum, i.e. it consists of one or more
-- products, combined pairwise with 'Plus' or 'Minus' operators:
exprParser :: Parsec String () Expr.Expr
exprParser = sumParser



parseExpr :: String -> Expr.Expr
parseExpr s = case parse exprParser "" s of
                   Left  err  -> error $ show err
                   Right expr -> expr
