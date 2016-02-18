module Lib
    ( solvePuzzle
    ) where

import Data.Bits
import Data.Ix
import Data.List
import Data.Maybe
import Data.Word
import Control.Applicative
import Control.Monad

type Puzzle = Word64

toPuzzle :: [Int] -> Word64
toPuzzle = foldl' (\r x -> unsafeShiftL r 4 + r) 0

{-# INLINE numAt #-}
--numAt :: Integral a => a -> a -> (a,a) -> Puzzle
numAt n p (i,j) = (unsafeShiftR p (4*(n*i + j))) .&. mask
    where mask = ((1 `unsafeShiftL` 4) - 1)

moves :: Integral a => Int -> Puzzle -> [(a, Puzzle)]
moves n p = catMaybes $ [swap] <*> deltas
    where numAt' = numAt n p
          valid = ((0,n-1),(0,n-1))
          z@(zi,zj) = head . filter ((== 0) . numAt') $ range valid
          deltas = [(zi+1,zj), (zi-1,zj), (zi,zj+1), (zi,zj-1)]
          swap b@(bi,bj)
            | not (inRange valid b) = Nothing
            | otherwise = Just (fromIntegral num, bitSwap)
              where bitSwap = p `xor`
                              (unsafeShiftL num (4*(index valid z))) `xor`
                              (unsafeShiftL num (4*(index valid b)))
                    num = numAt' b

solvePuzzle :: Int -> [Int] -> Maybe [Int]
solvePuzzle n xs = do
    guard $ sort xs == [0..n*n-1]
    return [6,5,8,7]
