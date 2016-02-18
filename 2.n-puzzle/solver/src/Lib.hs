module Lib
    ( solvePuzzle
    ) where

import Data.Bits
import Data.List
import Data.Maybe
import Data.Word
import Debug.Trace
import Control.Applicative
import Control.Monad

type Puzzle = Word64

toPuzzle :: [Int] -> Word64
toPuzzle = foldl' (\r x -> unsafeShiftL r 4 + fromIntegral x) 0

{-# INLINE numAt #-}
--numAt :: Integral a => a -> a -> (a,a) -> Puzzle
numAt n p (i,j) = (unsafeShiftR p (4*(n*i + j))) .&. mask
    where mask = ((1 `unsafeShiftL` 4) - 1)

moves :: Integral a => Int -> Puzzle -> [(a, Puzzle)]
moves n p = catMaybes $ [swap] <*> deltas
    where numAt' = numAt n p
          indices = [(i,j) | i <- [0..n-1], j <- [0..n-1]]
          z@(zi,zj) = head . filter ((== 0) . numAt') $ indices
          deltas = [(zi+1,zj), (zi-1,zj), (zi,zj+1), (zi,zj-1)]
          swap b@(bi,bj)
            | bi < 0 || bi >= n || bj < 0 || bj >= n = Nothing
            | otherwise = Just (fromIntegral num, bitSwap)
              where bitSwap = p `xor`
                              (unsafeShiftL num (4*(zi*n+zj))) `xor`
                              (unsafeShiftL num (4*(bi*n+bj)))
                    num = numAt' b

solvePuzzle :: Int -> [Int] -> Maybe [Int]
solvePuzzle n xs = do
    guard $ sort xs == [0..n*n-1]
    let first = head . moves n . snd
    let movs = take 4 . tail $ iterate first (0,(toPuzzle xs))
    return $ map fst movs
