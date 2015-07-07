import Control.Monad (join, liftM2)
import Data.List
import Data.Maybe (catMaybes)

-- | a data structure for machine-readable arithmetic expressions
data AExpr = IntConst !Integer
           | ABinary !ABinOp !AExpr !AExpr

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

allAExprs :: [Integer] -> [AExpr]
allAExprs xs = do
  op <- [Add, Subtract, Multiply, Divide]
  allAExprs' xs op

allAExprs' :: [Integer] -> ABinOp -> [AExpr]
allAExprs' xs op
  | length xs == 2 =
      [ ABinary op (IntConst $ head xs) (IntConst $ xs!!1)
      , ABinary op (IntConst $ xs!!1) (IntConst $ head xs)
      ]
  | otherwise = do
      x <- xs
      subAExpr <- allAExprs $ delete x xs
      [ ABinary op (IntConst x) subAExpr
        , ABinary op subAExpr (IntConst x)
        ] -- How do I make this pretty???

evalAExpr :: AExpr -> Maybe Rational
evalAExpr (IntConst x) = Just $ fromInteger x
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
         # takeWhile (uncurry (==))
         # length

main :: IO ()
main = getLine >> getLine >>= words # map read # result # print
