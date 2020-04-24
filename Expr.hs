
module Expr ( Op(..)
            , Expr(..)
            , isSum
            , isMonomial ) where


import Prelude hiding ((<>))
import Text.PrettyPrint


data Op = Times | Minus | Plus
          deriving (Eq, Ord)


prettyOp :: Op -> Doc
prettyOp Plus  = text "+"
prettyOp Minus = text "-"
prettyOp Times = text "*"


-- An 'Expr' represents a polynomial in variables (data constructor 'Variable').
-- Coefficients of variable terms in a polynomial are formed from constants
-- (data constructor 'Constant') and parameters (data constructor 'Parameter').
data Expr = BinOp Op Expr Expr
          | Neg Expr
          | Variable String
          | Parameter String
          | Constant Int
          deriving (Eq, Ord)


isSum :: Expr -> Bool
isSum (BinOp Minus _ _) = True
isSum (BinOp Plus  _ _) = True
isSum _                 = False


-- A monomial is a product of variables, parameters and constants.
-- Negations are also allowed inside a monomial:
isMonomial :: Expr -> Bool
isMonomial (BinOp Minus _ _) = False
isMonomial (BinOp Plus  _ _) = False
isMonomial (BinOp Times a b) =
  isMonomial a && isMonomial b
isMonomial (Neg expr) = isMonomial expr 
isMonomial _          = True


prettyVariable :: String -> Doc
prettyVariable s = (text $ s)


prettyParameter :: String -> Doc
prettyParameter s = (text "$") <> (text $ s)


prettyConstant :: Int -> Doc
prettyConstant i = text $ show i


prettyArg :: Bool -> Expr -> Doc
prettyArg ap e@(BinOp Times _ _) = prettyExpr ap e
prettyArg ap e@(BinOp _     _ _) = parens $ prettyExpr ap e
prettyArg ap e                   = prettyExpr ap e


prettyExpr :: Bool -> Expr -> Doc
-- Argument 'ap': "after (opening) parenthesis"
prettyExpr ap (BinOp Times left right) =
  (prettyArg ap left) <+> (prettyOp Times) <+> (prettyArg False right)
prettyExpr ap (BinOp Minus left right) =
  (prettyExpr ap left) <+> (prettyOp Minus) <+> (prettyArg False right)
prettyExpr ap (BinOp Plus left right) =
  (prettyExpr ap left) <+> (prettyOp Plus) <+> (prettyExpr False right)
prettyExpr ap (Neg expr) =
  let wrap = if ap then id else parens
  in wrap $ (text "-") <> (prettyExpr False expr)
prettyExpr _ (Variable s) = prettyVariable s
prettyExpr _ (Parameter s) = prettyParameter s
prettyExpr _ (Constant i) =
  let wrap = if i < 0 then parens else id
  in wrap $ prettyConstant i


prettyExprTopLevel :: Expr -> Doc
prettyExprTopLevel = prettyExpr True


-- For debugging: printing of fully parenthezised expressions.
prettyParenExpr :: Expr -> Doc
prettyParenExpr (BinOp op left right) = parens $
  (prettyParenExpr left) <+> (prettyOp op) <+> (prettyParenExpr right)
prettyParenExpr (Neg expr) = parens $ (text "-") <> prettyParenExpr expr
prettyParenExpr (Variable s) = prettyVariable s
prettyParenExpr (Parameter s) = prettyParameter s
prettyParenExpr (Constant i) =
  let wrap = if i < 0 then parens else id
  in wrap $ prettyConstant i


instance Show Expr where
  show = render . prettyParenExpr



                                     




