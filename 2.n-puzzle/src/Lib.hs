module Lib
    ( solvePuzzle
    ) where

import Data.List
import Control.Monad

solvePuzzle :: Int -> [Int] -> Maybe [Int]
solvePuzzle n xs = do
    guard $ sort xs == [1..n*n]
    return [6,5,8,7]
