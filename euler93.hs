import Control.Applicative ((<$>), (<*>))
import Control.Monad (join, liftM2)
import Data.List (group, inits, nub, permutations, sort, tails)
import Data.Maybe (catMaybes)

-- | a data structure for machine-readable arithmetic expressions
data AExpr = IntCon !Integer
           | ABin !ABinOp !AExpr !AExpr

-- | an auxiliary data structure for representing binary ops in AExpr
data ABinOp = Add | Sub | Mul | Div deriving Eq

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

infixl 9 #
(#) :: (a -> b) -> (b -> c) -> a -> c
-- ^ convenience infix op for forward function composition
(#) = flip (.)

targets :: [Integer] -> [Integer]
-- ^ returns the strictly increasing list of all target integers that
--   are obtainable from the given list of integers
targets = allAExprs
        # map evalAExpr
        # catMaybes
        # filter (\x -> x == (fromInteger . round $ x))
        # map round
        # nub
        # sort

result :: [Integer] -> Int
-- ^ returns the largest positive sequential integer obtainable from the
--   input list
result = targets
       # filter (> 0)
       # zip [1..]
       # takeWhile (uncurry (==))
       # length

main :: IO ()
main = getLine >> getLine >>= words # map read # result # print
