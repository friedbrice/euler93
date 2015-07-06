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
-- ^ evaluates an AExpr to a Rational, maybe
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
-- ^ builds list of AExprs from a given ordered list of integers
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
-- ^ all of our expressions in one list
allAExprs = concatMap opInsert . permutations

targets :: [Integer] -> [Integer]
-- ^ all the integers produced by our expressions
targets = allAExprs
        # map evalAExpr
        # catMaybes
        # sort
        # filter (\x -> x == (fromInteger . round $ x))
        # map round
        # group
        # map head

posSeq :: [Integer] -> [Integer]
-- ^ only the sequential positive targets
posSeq = targets
       # filter (> 0)
       # zip [1..]
       # takeWhile (\(a,b) -> a == b)
       # map snd

result :: [Integer] -> Integer
-- ^ output congenial to the problem statement
result ints = if posSeq ints == []
    then 0
    else last . posSeq $ ints

main :: IO ()
main = getLine >> getLine >>= words # map read # result # print
