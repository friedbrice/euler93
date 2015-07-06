import Control.Monad
import Data.List

sizes :: [[FormTree]]
sizes = [Empty] : (map go . drop 1 . inits) sizes where
    go smaller = do
      (ls, rs) <- zip smaller (reverse smaller)
      liftM2 Node ls rs
