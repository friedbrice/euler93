import Control.Applicative (pure, (<*>), (<$>))
import Data.List (inits, tails)

data Tree x = Null
            | Leaf x
            | Node (Tree x) x (Tree x)
    deriving Show

splits :: [a] -> [([a], [a])]
splits xs = zip (inits xs) (tails xs)

getAllTrees :: [Int] -> [Tree Int]
getAllTrees []  = [Null]
getAllTrees [x] = [Leaf x]
getAllTrees xs  = do
    (left, x:right) <- splits xs
    Node <$> getAllTrees left <*> pure x <*> getAllTrees right
