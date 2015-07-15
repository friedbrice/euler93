import Control.Applicative (liftA2, (<*>))
import Data.List (inits, nub, permutations, sort, tails)
import Data.Maybe (catMaybes)
import Data.Ratio (numerator, denominator)

opInsert :: [Rational] -> [Maybe Rational]
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
      where
        splits xs = init . tail $ zip (inits xs) (tails xs)

targets :: [Integer] -> [Integer]
targets = sort . nub
        . map numerator . filter ((1 ==) . denominator)
        . catMaybes . concatMap opInsert
        . permutations . map fromIntegral

result :: [Integer] -> Int
result = length . takeWhile (uncurry (==)) . zip [1..]
       . filter (>0) . targets

main :: IO ()
main = getLine >> getLine >>= print . result . map read . words
