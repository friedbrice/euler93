import Control.Monad (join, liftM2)
import Data.List (group, permutations, sort)
import Data.Maybe (catMaybes)

infixl 9 #
(#) :: (a -> b) -> (b -> c) -> a -> c
-- ^ convenience infix op for reverse function composition
(#) = flip (.)

-- | a data structure for machine-readable arithmetic expressions
data AExpr = IntCon !Integer
           | ABin !ABinOp !AExpr !AExpr

-- | an auxiliary data structure for representing binary ops in AExpr
data ABinOp = Add
            | Sub
            | Mul
            | Div

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
evalAExpr (IntCon x)     = Just $ fromInteger x
evalAExpr (ABin Add l r) = liftM2 (+) (evalAExpr l) (evalAExpr r)
evalAExpr (ABin Sub l r) = liftM2 (-) (evalAExpr l) (evalAExpr r)
evalAExpr (ABin Mul l r) = liftM2 (*) (evalAExpr l) (evalAExpr r)
evalAExpr (ABin Div l r) = join $ liftM2 safeDiv l' r'
  where
    l' = evalAExpr l
    r' = evalAExpr r
    safeDiv :: Rational -> Rational -> Maybe Rational
    safeDiv _ 0 = Nothing
    safeDiv a b = Just $ a / b

opInsert :: [Integer] -> [AExpr]
-- ^ returns list of all AExprs that use each member of the given list,
--   in the order given, exactly once
opInsert []       = []
opInsert (x:[])   = [IntCon x]
opInsert (x:y:zs) = do
    op <- [Add, Sub, Mul, Div]
    if null zs
        then return $ ABin op (IntCon x) (IntCon y)
        else do
            second <- opInsert (y:zs)
            return $ ABin op (IntCon x) second

allAExprs :: [Integer] -> [AExpr]
-- ^ returns list of all AExprs that use each member of the given list,
--   in any order, exactly once
allAExprs = concatMap opInsert . permutations

targets :: [Integer] -> [Integer]
-- ^ returns the strictly increasing list of all target integers that
--   are obtainable from the given list of integers
targets = allAExprs
        # map evalAExpr
        # catMaybes
        # sort
        # filter (\x -> x == (fromInteger . round $ x))
        # map round
        # group
        # map head

result :: [Integer] -> Int
-- ^ returns the largest positive sequential integer obtainable from the
--   input list
result = targets
       # filter (> 0)
       # zip [1..]
       # takeWhile (\(a,b) -> a == b)
       # map snd
       # length

main :: IO ()
main = getLine >> getLine >>= words # map read # result # print
