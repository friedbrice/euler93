import Control.Applicative ((<$>), (<*>))
import Control.Monad (join, liftM2)
import Data.List (inits, permutations, tails)
import Data.Maybe (catMaybes)

-- | a data structure for machine-readable arithmetic expressions
data AExpr = IntCon !Integer
           | ABin !ABinOp !AExpr !AExpr

-- | an auxiliary data structure for representing binary ops in AExpr
data ABinOp = Add | Sub | Mul | Div deriving Eq

-- | replaces an ABinOp token with its common symbol
instance Show (ABinOp) where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

-- | converts an AExpr into human-readable arithmetic
instance Show (AExpr) where
    show (IntCon n)    = show n
    show (ABin op l r) = "(" ++ show l ++ show op ++ show r ++ ")"

evalAExpr :: AExpr -> Maybe Rational
-- ^ safely evaluates an AExpr to a Maybe Rational
evalAExpr (IntCon x) = Just $ fromInteger x
evalAExpr (ABin op l r)
    | op == Add = liftM2 (+) l' r'
    | op == Sub = liftM2 (-) l' r'
    | op == Mul = liftM2 (*) l' r'
    | op == Div = join $ liftM2 safeDiv l' r'
      where
        l' = evalAExpr l
        r' = evalAExpr r
        safeDiv _ 0 = Nothing
        safeDiv a b = Just $ a / b

opInsert :: [Integer] -> [AExpr]
-- ^ returns list of all AExprs that use each member of the given list,
--   in the order given, exactly once
opInsert [x] = [IntCon x]
opInsert xs  = do
    (ls, rs) <- splits xs
    ABin <$> [Add, Sub, Mul, Div] <*> opInsert ls <*> opInsert rs
      where
        splits xs = init . tail $ zip (inits xs) (tails xs)

allAExprs :: [Integer] -> [AExpr]
-- ^ returns list of all AExprs that use each member of the given list,
--   in any order, exactly once
allAExprs = concatMap opInsert . permutations
