{-# OPTIONS_GHC -funbox-strict-fields #-}

import Control.Monad (join, liftM2)
import Data.List
import Data.Maybe (catMaybes)

-- | a data structure for machine-readable arithmetic expressions
data AExpr = IntConst !Int
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

allAExprs :: [Int] -> [AExpr]
allAExprs xs
  | length xs == 1 = return $ IntConst $ head xs
  | otherwise = [Add, Subtract, Multiply, Divide] >>= allAExprs' xs

allAExprs' :: [Int] -> ABinOp -> [AExpr]
allAExprs' xs op
  | length xs == 2 =
    let x = IntConst $ head xs
        y = IntConst $ xs!!1
    in [ ABinary op x y, ABinary op y x ]
  | otherwise = do
      x <- xs
      let leaf = IntConst x
      allAExprs (delete x xs) >>=
        \y -> [ ABinary op leaf y
              , ABinary op y leaf
              ]

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

targets :: [Int] -> [Int]
-- ^ returns the list of all target integers that are obtainable from the given
-- list of integers
targets = allAExprs
          # map evalAExpr
          # catMaybes
          # filter (\x -> x == (fromInteger . round $ x))
          # map round
          # nub

result :: [Int] -> Int
-- ^ returns the largest positive sequential integer obtainable from the input
-- list
result = targets
         # (sort . filter (> 0))
         # zip [1..]
         # takeWhile (uncurry (==))
         # length

main :: IO ()
main = getLine >> getLine >>= words # map read # result # print
