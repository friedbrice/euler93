import Control.Applicative
import Data.List

splits :: [a] -> [([a], [a])]
splits xs = init . tail $ zip (inits xs) (tails xs)

applyOp :: (a -> b -> c) -> a -> b -> c
applyOp op a1 a2 = op a1 a2

opInsert :: [Float] -> [Float]
opInsert (x:[])   = [x]
opInsert ints     = do
    (ls, rs) <- splits ints
    applyOp <$> [(+), (-), (*), (/)] <*> opInsert ls <*> opInsert rs

allAExprs :: [Float] -> [Float]
allAExprs = concatMap opInsert . permutations

