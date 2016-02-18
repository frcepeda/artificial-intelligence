module Lib
    ( solvePuzzle
    ) where

import Utils
import Data.Bits
import Data.List
import Data.Maybe
import qualified Data.IntSet as S
import qualified Data.PQueue.Min as P
import Data.Word
import Debug.Trace
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Strict

type Puzzle = Int -- Word64

pprint n p = show $ map (map (numAt n p)) [[(i,j) | j <- [0..n-1]] | i <- [0..n-1]]

toPuzzle :: [Int] -> Puzzle
toPuzzle = foldr (\x r -> unsafeShiftL r 4 + fromIntegral x) 0

{-# INLINE numAt #-}
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
    aStar n (toPuzzle xs) (toPuzzle [0..n*n-1])

type AStar a = State (S.IntSet, P.MinQueue (Int, [Int], Puzzle)) a

aStar :: Int -> Puzzle -> Puzzle -> Maybe [Int]
aStar n start goal = evalState aStar' (S.empty, (P.singleton (0,[],start)))
    where aStar' = do
            (vis, queue) <- get
            case P.minView queue of
                Nothing -> return Nothing
                (Just ((c,p,x), newQueue)) -> do
                       let newVis = S.insert x vis
                       put (newVis, newQueue)

                       if x == goal
                         then return $ Just (reverse p)
                         else do forM_ (moves n x) $ \(m, next) -> do
                                     unless (next `S.member` newVis) $ do 
                                          modify (mapSnd $ P.insert (c+1, m:p, next))
                                 aStar'
