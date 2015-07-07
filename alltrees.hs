import Data.List (inits, tails)

data Tree x = Null
            | Leaf x
            | Node (Tree x) x (Tree x)

splits :: [a] -> [([a], [a])]
splits xs = zip (inits xs) (tails xs)

getAllTrees :: [Int] -> [Tree Int]
getAllTrees [] = [Null]
getAllTrees [x] = [Leaf x]
