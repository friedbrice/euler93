import           Control.Applicative
import           Control.Monad       (join, liftM2)
import           Data.List
import           Data.Maybe          (catMaybes)

-- | a data structure for machine-readable arithmetic expressions
data AExpr = IntConst Integer
           | ABinary ABinOp AExpr AExpr
           deriving Eq

-- | an auxiliary data structure for representing binary ops in AExpr
data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
            deriving Eq

-- | replaces an ABinOp token with its common symbol
instance Show (ABinOp) where
  show Add = "+"
  show Subtract = "-"
  show Multiply = "*"
  show Divide = "/"

-- | converts an AExpr into human-readable arithmetic
instance Show (AExpr) where
  show (IntConst n) = show n
  show (ABinary op l r) = "(" ++ show l ++ show op ++ show r ++ ")"

splits :: [a] -> [([a], [a])]
-- ^ utility function returns list of all cuts of the given list
splits xs = init . tail $ zip (inits xs) (tails xs)

opInsert :: [Integer] -> [AExpr]
-- ^ returns list of all AExprs that use each member of the given list,
--   in the order given, exactly once
opInsert (x:[])   = [IntConst x]
opInsert ints     = do
    (ls, rs) <- splits ints
    op <- ABinary <$> [Add, Subtract, Multiply, Divide]
    lss <- opInsert ls
    rss <- opInsert rs
    return $ op lss rss

allAExprs :: [Integer] -> [AExpr]
allAExprs = concatMap opInsert . permutations

evalAExpr :: AExpr -> Maybe Rational
evalAExpr (IntConst x) = Just $ fromIntegral x
evalAExpr (ABinary op l r)
  | op /= Divide = liftM2 f l' r'
  | otherwise = join $ liftM2 safeDiv l' r'
  where f = case op
            of Add -> (+)
               Subtract -> (-)
               Multiply -> (*)
        l' = evalAExpr l
        r' = evalAExpr r
        safeDiv _ 0 = Nothing
        safeDiv a b = Just $ a / b

infixl 9 #
(#) :: (a -> b) -> (b -> c) -> a -> c
-- ^ convenience infix op for reverse function composition
(#) = flip (.)

targets :: [Integer] -> [Integer]
-- ^ returns the list of all target integers that are obtainable from the given
-- list of integers
targets = allAExprs
          # map evalAExpr
          # catMaybes
          # filter (\x -> x == (fromInteger . round $ x))
          # map round
          # nub

result :: [Integer] -> Int
-- ^ returns the largest positive sequential integer obtainable from the input
-- list
result = targets
         # (sort . filter (> 0))
         # zip [1..]
         # takeWhile (uncurry (==))
         # length

main :: IO ()
main = getLine >> getLine >>= words # map read # result # print
