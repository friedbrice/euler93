import Control.Applicative
import Control.Monad
import Data.List

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
splits xs = init . tail $ zip (inits xs) (tails xs)

opInsert :: [Integer] -> [AExpr]
-- ^ returns list of all AExprs that use each member of the given list,
--   in the order given, exactly once
opInsert (x:[])   = [IntCon x]
opInsert ints     = do
    (ls, rs) <- splits ints
    ABin <$> [Add, Sub, Mul, Div] <*> opInsert ls <*> opInsert rs

allAExprs :: [Integer] -> [AExpr]
allAExprs = concatMap opInsert . permutations

allAExprs2 :: [Integer] -> [AExpr]
allAExprs2 (x:[])   = [IntCon x]
allAExprs2 ints     = do
    (ls, rs) <- splits ints
    let ops = [Add, Sub, Mul, Div]
        lss = allAExprs2 ls
        rss = allAExprs2 rs
    (ABin <$> ops <*> lss <*> rss) ++ (ABin <$> ops <*> rss <*> lss)
