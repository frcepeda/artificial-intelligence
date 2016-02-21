{-# LANGUAGE TypeFamilies #-}

module Lib
    ( solvePuzzle
    , patternDB
    ) where

import Utils
import Data.Bits
import Data.List
import Data.Maybe
import Data.STRef
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as V
import qualified Data.IntSet as S
import qualified Data.IntMap.Strict as M
import qualified Data.PQueue.Min as P
import Data.Word
import Debug.Trace
import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Trans.State.Strict

type Puzzle = Int -- Word64

pprint n p = show $ map (map (numAt n p)) [[(i,j) | j <- [0..n-1]] | i <- [0..n-1]]

toPuzzle :: [Int] -> Puzzle
toPuzzle = foldr (\x r -> unsafeShiftL r 4 + fromIntegral x) 0

{-# INLINE numAt #-}
numAt n p (i,j) = (unsafeShiftR p (4*(n*i + j))) .&. mask
    where mask = ((1 `unsafeShiftL` 4) - 1)

{-# INLINE findElem #-}
findElem n p e = head . filter ((== e) . numAt n p) $ indices
    where indices = [(i,j) | i <- [0..n-1], j <- [0..n-1]]

moves :: Integral a => Int -> Puzzle -> [(a, Puzzle)]
moves n p = catMaybes $ [swap] <*> deltas
    where z@(zi,zj) = findElem n p 0
          deltas = [(zi+1,zj), (zi-1,zj), (zi,zj+1), (zi,zj-1)]
          swap b@(bi,bj)
            | bi < 0 || bi >= n || bj < 0 || bj >= n = Nothing
            | otherwise = Just (fromIntegral num, bitSwap)
              where bitSwap = p `xor`
                              (unsafeShiftL num (4*(zi*n+zj))) `xor`
                              (unsafeShiftL num (4*(bi*n+bj)))
                    num = numAt n p b

solvePuzzle :: Int -> [Int] -> V.Vector Word8 -> Maybe [Int]
solvePuzzle n xs db = do
    guard $ sort xs == [0..n*n-1]
    let h = if n < 4 then manhattanDist n else pCost db
    aStar n h (toPuzzle xs) (toPuzzle [0..n*n-1])

manhattanDist n p = 0
pCost db p = db V.! patternID p

type AStar a = State (S.IntSet, P.MinQueue (Int, [Int], Puzzle)) a

aStar :: Int -> Puzzle -> Puzzle -> Maybe [Int]
aStar n h start goal = evalState aStar' (S.empty, P.singleton (0,[],start))
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

patternDB = bfs 4 (toPuzzle [0,1,1,3,1,1,1,7,1,1,1,11,12,13,14,15])

dbSize = factorial 16 `div` factorial 8

patternID :: Puzzle -> Int
patternID p = permutationNumber (length nums) nums
    where markers = [0,3,7,11,12,13,14,15]
          locs = map (findElem 4 p) markers
          nums = map (\(r,c) -> r*4 + c) locs

permutationNumber _ [] = 0
permutationNumber n (x:xs) =
    x * factorial n + permutationNumber (n-1) (shiftLess x xs)
    where shiftLess x = map (\y -> if y < x then y else y-1)

bfs :: Int -> Puzzle -> V.Vector Word8
bfs n start = runST $ do
    vis <- GM.new dbSize :: ST s (V.MVector (PrimState (ST s)) Bool)
    db <- GM.new dbSize
    q <- newSTRef (P.singleton (0, start))
    let bfs' = do
         queue <- readSTRef q
         case P.minView queue of
             Nothing -> return ()
             (Just ((c,x), newQueue)) -> do
                 writeSTRef q newQueue
                 GM.write vis (patternID x) True
                 GM.write db (patternID x) c

                 forM_ (moves n x) $ \(m, next) -> do
                     isMember <- GM.read vis (patternID next)
                     unless isMember $ do 
                         modifySTRef q (P.insert (c+1, next))

                 bfs'
    bfs'
    V.freeze db
