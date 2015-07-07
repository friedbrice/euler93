import Control.Applicative (pure, (<*>), (<$>))
import Data.List (inits, tails)

-- | a data structure for machine-readable arithmetic expressions
data AExpr = IntCon !Integer
           | ABin !ABinOp !AExpr !AExpr

-- | an auxiliary data structure for representing binary ops in AExpr
data ABinOp = Add
            | Sub
            | Mul
            | Div
    deriving Eq

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

splits :: [a] -> [([a], [a])]
-- ^ utility function returns list of all cuts of the given list
splits xs = zip (inits xs) (tails xs)

opInsert :: [Integer] -> [AExpr]
-- ^ returns list of all AExprs that use each member of the given list,
--   in the order given, exactly once
opInsert []     = []
opInsert (x:[]) = [IntCon x]
opInsert ints   = do
    (ls, rs) <- splits ints
    ABin <$> [Add, Sub, Mul, Div] <*> opInsert ls <*> opInsert rs
