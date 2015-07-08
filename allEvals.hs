import Control.Applicative (liftA2, (<*>))
import Data.List (inits, nub, permutations, sort, tails)
import Data.Maybe (catMaybes)

splits :: [a] -> [([a], [a])]
-- ^ returns all way to cut the given list into two non-empty lists
splits xs = init . tail $ zip (inits xs) (tails xs)

opInsert :: [Rational] -> [Maybe Rational]
-- ^ returns all arithmetic combinations of the numbers from the given
--   list in the order given.
opInsert [x]  = [Just x]
opInsert ints = do
    (ls, rs) <- splits ints
    let p = liftA2 (+)
        m = liftA2 (-)
        t = liftA2 (*)
        d = safeDiv
          where
            safeDiv Nothing  _        = Nothing
            safeDiv _        Nothing  = Nothing
            safeDiv _        (Just 0) = Nothing
            safeDiv (Just a) (Just b) = Just $ a / b
    [p, m, t, d] <*> opInsert ls <*> opInsert rs

allEvals :: [Rational] -> [Maybe Rational]
-- ^ returns all arithmetic combinations of the numbers from the given
--   list in any order.
allEvals = concatMap opInsert . permutations

infixl 9 #
(#) :: (a -> b) -> (b -> c) -> a -> c
-- ^ convenience infix op for reverse function composition
(#) = flip (.)

targets :: [Int] -> [Int]
-- ^ returns the list of all target integers that are obtainable from
--   the given list of integers
targets = map fromIntegral
          # allEvals
          # catMaybes
          # filter (\x -> x == (fromInteger . round $ x))
          # map round
          # nub

result :: [Int] -> Int
-- ^ returns the largest positive sequential integer obtainable from the
--   given list of integers
result = targets
         # (sort . filter (> 0))
         # zip [1..]
         # takeWhile (uncurry (==))
         # length

main :: IO ()
main = getLine >> getLine >>= words # map read # result # print
