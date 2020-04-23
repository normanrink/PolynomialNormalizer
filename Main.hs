
module Main where


import System.IO

import qualified Expr
import qualified Parser
import qualified Transformations as Trafos


main = do
  input <- getLine
  let expr = Parser.parseExpr input
  putStrLn . show $ Trafos.normalizeAllMonomialsL expr
  putStrLn . show $ Trafos.normalizeAllMonomialsR expr
  putStrLn "---------------------------------------"
  putStrLn . show $ Trafos.normalizeL expr
  putStrLn . show $ Trafos.normalizeR expr
  putStrLn "---------------------------------------"

