import Control.Applicative
import Data.List

splits :: [a] -> [([a], [a])]
splits xs = init . tail $ zip (inits xs) (tails xs)

opInsert :: [Rational] -> [Rational]
opInsert [x]  = [x]
opInsert ints = do
    (ls, rs) <- splits ints
    [(+), (-), (*), (/)] <*> opInsert ls <*> opInsert rs

allEvals :: [Rational] -> [Rational]
allEvals = concatMap opInsert . permutations
