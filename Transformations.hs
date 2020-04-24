
module Transformations ( distribute
                       , flipSign
                       , removeMinus
                       , removeNeg
                       , leftAssoc
                       , rightAssoc
                       , normalizeAllMonomialsL
                       , normalizeAllMonomialsR
                       , normalizeL
                       , normalizeR ) where


import Data.List

import Expr


distribute :: Expr -> Expr
distribute (BinOp Times left right) =
  let left'  = distribute left
      right' = distribute right
  in if isSum left'
     then case left' of
          BinOp op a b -> --  (a 'op' b) * right ~> (a * right) 'op' (b * right) :
                          (BinOp op (distribute $ BinOp Times a right)
                                    (distribute $ BinOp Times b right))
     else if isSum right'
          then case right' of
               BinOp op a b -> --  left * (a 'op' b) ~> (left * a) 'op' (left * b) :
                               (BinOp op (distribute $ BinOp Times left a)
                                         (distribute $ BinOp Times left b))
          else (BinOp Times left' right')
distribute (BinOp op left right) =
  BinOp op (distribute left) (distribute right)
distribute (Neg e) = Neg (distribute e)
distribute e = e


flipSign :: Bool -> Expr -> Expr
flipSign leftFlip (BinOp Times left right) =
  if leftFlip
     then BinOp Times (flipSign leftFlip left) right
     else BinOp Times left (flipSign leftFlip right)
flipSign leftFlip (BinOp Minus left right) =
  BinOp Plus (flipSign leftFlip left) right
flipSign leftFlip (BinOp Plus left right) =
  BinOp Minus (flipSign leftFlip left) right
flipSign _ (Neg expr) = expr
flipSign _ (Variable s) = (Neg $ Variable s)
flipSign _ (Parameter s) = (Neg $ Parameter s)
flipSign _ (Constant i) = Constant (negate i)


removeMinus :: Expr -> Expr
removeMinus (BinOp Minus left right) =
  BinOp Plus (removeMinus left) (removeMinus $ flipSign True right)
removeMinus (BinOp op left right) =
  BinOp op (removeMinus left) (removeMinus right)
removeMinus (Neg expr) = Neg (removeMinus expr)
removeMinus e = e


removeNeg :: Expr -> Expr
removeNeg (BinOp op left right)
  = BinOp op (removeNeg left) (removeNeg right)
removeNeg (Neg expr) = BinOp Times (Constant $ negate 1) (removeNeg expr)
removeNeg e = e


leftAssoc :: Expr -> Expr
leftAssoc (BinOp Plus left right) =
  let left'  = leftAssoc left
      right' = leftAssoc right
  in case right' of
     (BinOp Plus  a b) -> --  left' + (a + b) ~> (leftAssoc $ left' + a) + b :
                          BinOp Plus  (leftAssoc $ BinOp Plus left' a) b
     (BinOp Minus a b) -> --  left' + (a - b) ~> (leftAssoc $ left' + a) - b :
                          BinOp Minus (leftAssoc $ BinOp Plus left' a) b
     _ -> BinOp Plus left' right'
leftAssoc (BinOp Minus left right) =
  let left'  = leftAssoc left
      right' = leftAssoc right
  in case right' of
     (BinOp Plus  a b) -> --  left' - (a + b) ~> (leftAssoc $ left' - a) - b :
                          BinOp Minus (leftAssoc $ BinOp Minus left' a) b
     (BinOp Minus a b) -> --  left' - (a - b) ~> (leftAssoc $ left' - a) + b :
                          BinOp Plus  (leftAssoc $ BinOp Minus left' a) b
     _ -> BinOp Minus left' right'
leftAssoc (BinOp Times left right) =
  let left'  = leftAssoc left
      right' = leftAssoc right
  in case right' of
     (BinOp Times a b) -> --  left' * (a * b) ~> (leftAssoc $ left' * a) * b :
                          BinOp Times (leftAssoc $ BinOp Times left' a) b
     _ -> BinOp Times left' right'
leftAssoc (Neg expr) = Neg (leftAssoc expr)
leftAssoc e = e


rightAssoc :: Expr -> Expr
rightAssoc (BinOp Plus left right) =
  let left'  = rightAssoc left
      right' = rightAssoc right
  in case left' of
     (BinOp Plus  a b) -> --  (a + b) + right' ~> a + (rightAssoc $ b + right') :
                          BinOp Plus  a (rightAssoc $ BinOp Plus  b right')
     (BinOp Minus a b) -> --  (a - b) + right' ~> a - (rightAssoc $ b - right') :
                          BinOp Minus a (rightAssoc $ BinOp Minus b right')
     _ -> BinOp Plus left' right'
rightAssoc (BinOp Minus left right) =
  let left'  = rightAssoc left
      right' = rightAssoc right
  in case left' of
     (BinOp Plus  a b) -> --  (a + b) - right' ~> a + (rightAssoc $ b - right') :
                          BinOp Plus  a (rightAssoc $ BinOp Minus b right')
     (BinOp Minus a b) -> --  (a - b) - right' ~> a - (rightAssoc $ b + right') :
                          BinOp Minus a (rightAssoc $ BinOp Plus  b right')
     _ -> BinOp Minus left' right'
rightAssoc (BinOp Times left right) =
  let left'  = rightAssoc left
      right' = rightAssoc right
  in case left' of
     (BinOp Times a b) -> --  (a * b) * right' ~> a * (rightAssoc $ b * right') :
                          BinOp Times a (rightAssoc $ BinOp Times b right')
     _ -> BinOp Times left' right'
rightAssoc (Neg expr) = Neg (rightAssoc expr)
rightAssoc e = e


collectVariables :: Expr -> [String]
collectVariables (BinOp _ a b) =
  collectVariables a ++ collectVariables b
collectVariables (Neg expr) = collectVariables expr
collectVariables (Variable s) = [s]
collectVariables _            = []


collectParameters :: Expr -> [String]
collectParameters (BinOp _ a b) =
  collectParameters a ++ collectParameters b
collectParameters (Neg expr) = collectParameters expr
collectParameters (Parameter s) = [s]
collectParameters _             = []


collectConstants :: Expr -> [Int]
collectConstants (BinOp _ a b) =
  collectConstants a ++ collectConstants b
collectConstants (Neg expr)   = (negate 1):collectConstants expr
collectConstants (Constant i) = [i]
collectConstants _            = []


-- An 'Expr' can represent a polynomial in fully unconstraint form. For example,
-- in a general 'Expr', occurrences of the 'Times' operator (for multiplication)
-- do not need to be distributed out.
-- Polynomials can be organized into more standard forms. For example, a polynomial
-- is a sum of monomials, where each monomial is itself a product of variables
-- multiplied with a coefficient (formed from constants and parameters).
-- The process of transforming terms or expressions into a standard form is usually
-- referred to as normalization. The remainder of this source file is concerned
-- with normalizing polynomials (and thus also with normalizing monomials).

-- A 'Coefficient' represents that part of a monomial that consists of
-- constants and parameters. The product of constants has been folded
-- into a single 'factor':
data Coefficient = Coefficient { factor     :: Int
                               , parameters :: [String] }


-- A 'Coefficient' is normalized iff the 'parameters' it contains are sorted:
normalizeCoefficient :: Coefficient -> Coefficient
normalizeCoefficient (Coefficient factor parameters) = Coefficient factor (sort parameters)


-- In a list of coefficients, combine all those that have the same parameters:
mergeCoefficients :: [Coefficient] -> [Coefficient]
mergeCoefficients cs = let params = nub $ map (sort . parameters) cs
                           pred   = \ps coeff -> (sort . parameters $ coeff) == ps
                           combineFactors = foldr (+) 0
                           merge = \ps -> let factors = map factor $ filter (pred ps) cs
                                          in combineFactors factors
                       in [(Coefficient (merge ps) ps) | ps <- params]


-- Conventionally, a monomial is a product of variables and a coefficient. The data
-- structure 'Monomial' allows for a more general representation of monomials, where
-- the variables are multiplied with a sum of coefficients: (Hence the list of
-- 'Coefficient' in the following definition.)
data Monomial = Monomial { coeffs    :: [Coefficient]
                         , variables :: [String] }


-- A 'Monomial' is normalized iff
-- (a) the variables it contains are sorted,
-- (b) coefficients with the same parameters have been combined (i.e. merged), are
-- (c) the (combined) coefficients are normalized.
normalizeMonomial :: Monomial -> Monomial
normalizeMonomial (Monomial coeffs variables) =
  let variables' = sort variables
      coeffs' = mergeCoefficients coeffs
  in Monomial (map normalizeCoefficient coeffs') variables'


-- In a list of monomials, combine all those that have the same variables:
-- (Note that the coefficients are NOT merged in this process.)
mergeMonomials :: [Monomial] -> [Monomial]
mergeMonomials ms = let vars = nub $ map (sort . variables) ms
                        pred = \vs mono -> (sort . variables $ mono) == vs
                        cs   = \vs -> concat $ map coeffs $ filter (pred vs) ms
                    in [(Monomial (cs vs) vs) | vs <- vars]


combineConstants :: [Int] -> Int
combineConstants = foldr (*) 1



collectMonomial :: Expr -> Monomial
collectMonomial e | isMonomial e = let factor = combineConstants (collectConstants e)
                                       params = collectParameters e
                                       coeff  = Coefficient factor params
                                       vars   = collectVariables e
                                   in Monomial [coeff] vars
                  | otherwise = undefined


collectMonomials :: Expr -> [Monomial]
collectMonomials e | isMonomial e = [collectMonomial e]
                   -- The guard 'otherwise' implies that 'e' is a sum
                   -- (i.e. 'op' in the following is 'Plus'/'Minus'):
                   | otherwise    = case e of
                       (BinOp op a b) -> (collectMonomials a) ++ (collectMonomials b)
                       (Neg _ ) -> undefined -- Negation should have been removed.
                       _ -> undefined


exprFromCoefficientL :: Coefficient -> Expr
exprFromCoefficientL (Coefficient factor params) =
  let initE = Constant factor
  in foldl (\l p -> BinOp Times l (Parameter p)) initE params


exprFromCoefficientR :: Coefficient -> Expr
exprFromCoefficientR (Coefficient factor params) =
  let initE = Constant factor
  in foldr (\p r -> BinOp Times (Parameter p) r) initE params


exprFromVariablesL :: Expr -> [String] -> Expr
exprFromVariablesL initE vars = foldl (\l v -> BinOp Times l (Variable v)) initE vars


exprFromVariablesR :: Expr -> [String] -> Expr
exprFromVariablesR initE vars = foldr (\v r -> BinOp Times (Variable v) r) initE vars


exprFromMonomialL :: Monomial -> Expr
exprFromMonomialL (Monomial []     _)    = undefined
-- Note that a monomial may have no variables (i.e. 'vars == []' in the following).
-- In this case, the monomial simply consists of its coefficent (formed from contants
-- and parameters).
exprFromMonomialL (Monomial (c:cs) vars) =
  let initE  = exprFromCoefficientL c
      initE' = foldl (\l c -> BinOp Plus l (exprFromCoefficientL c)) initE cs
  in exprFromVariablesL initE' vars


exprFromMonomialR :: Monomial -> Expr
exprFromMonomialR (Monomial []     _)    = undefined
-- Note that a monomial may have no variables (i.e. 'vars == []' in the following).
-- In this case, the monomial simply consists of its coefficent (formed from contants
-- and parameters).
exprFromMonomialR (Monomial (c:cs) vars) =
  let initE  = exprFromCoefficientR c
      initE' = foldr (\c r -> BinOp Plus (exprFromCoefficientR c) r) initE cs
  in exprFromVariablesR initE' vars


-- Turn an expression that is a monomial into
-- a standard (i.e. normal), left-associating form:
normalizeMonomialExprL :: Expr -> Expr
normalizeMonomialExprL e | isMonomial e = let m = collectMonomial e
                                          in (exprFromMonomialL . normalizeMonomial) m
                         | otherwise = undefined


-- Turn an expression that is a monomial into
-- a standard (i.e. normal), right-associating form:
normalizeMonomialExprR :: Expr -> Expr
normalizeMonomialExprR e | isMonomial e = let m = collectMonomial e
                                          in (exprFromMonomialR . normalizeMonomial) m
                         | otherwise = undefined


-- Apply a normalizer for a monomial expression (i.e. argument 'mono')
-- to all monomials in an expression (i.e. to all monomials in argument 'e'):
normalizeAllMonomials :: (Expr -> Expr) -> Expr -> Expr
normalizeAllMonomials mono e | isMonomial e = mono e
                             -- The guard 'otherwise' implies that 'e' is a sum
                             -- (i.e. 'op' in the following is 'Plus'/'Minus'):
                             | otherwise    = case e of
                                 (BinOp op a b) -> BinOp op (normalizeAllMonomials mono a)
                                                            (normalizeAllMonomials mono b)
                                 (Neg _) -> undefined -- Negation should have been removed.
                                 _ -> undefined


-- The following functions 'normalizeAllMonomials{L|R}' apply normalization
-- to all monomials in an expression individually (i.e. monomials are not
-- combined/merged based if they contains the same variables):

normalizeAllMonomialsL :: Expr -> Expr
normalizeAllMonomialsL = let normalize = normalizeAllMonomials normalizeMonomialExprL
                         in normalize . leftAssoc . distribute . removeNeg


normalizeAllMonomialsR :: Expr -> Expr
normalizeAllMonomialsR = let normalize = normalizeAllMonomials normalizeMonomialExprR
                         in normalize . rightAssoc . distribute .removeNeg


-- The following functions 'normalize{L|R}' normalize an entire polynomial 'e'.
-- This means that monomials are combined so that each product of variables that
-- occurs in 'e' occurs in exactly one monomial in the resulting expression:

normalizeL :: Expr -> Expr
normalizeL e =
  let e'    = (leftAssoc . distribute . removeNeg . removeMinus) e
      m0:ms = ((map normalizeMonomial) . mergeMonomials . collectMonomials) e'
      initE = exprFromMonomialL m0
  in -- Since 'Minus' operators have been removed from the argument expression 'e',
     -- the resulting expression can be rebuilt with 'Plus' only:
    foldl (\l m -> BinOp Plus l (exprFromMonomialL m)) initE ms


normalizeR :: Expr -> Expr
normalizeR e =
  let e'    = (rightAssoc . distribute . removeNeg . removeMinus) e
      m0:ms = ((map normalizeMonomial) . mergeMonomials . collectMonomials) e'
      initE = exprFromMonomialR m0
  in -- Since 'Minus' operators have been removed from the argument expression 'e',
     -- the resulting expression can be rebuilt with 'Plus' only:
    foldr (\m r -> BinOp Plus (exprFromMonomialR m) r) initE ms






  
  
