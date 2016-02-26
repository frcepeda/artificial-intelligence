{-# LANGUAGE TypeFamilies #-}

module Lib
    ( solvePuzzle
    , patternDB
    , fringe
    , corner
    ) where

import Utils
import qualified Queue as Q
import Data.Bits
import Data.List
import Data.Maybe
import Data.STRef
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Set as S
import qualified Data.PQueue.Min as P
import Data.Word
import Debug.Trace
import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Trans.State.Strict

type Puzzle = Word64

pprint n p = show $ map (map (numAt n p)) [[(i,j) | j <- [0..n-1]] | i <- [0..n-1]]

toPuzzle :: [Int] -> Puzzle
toPuzzle = foldr (\x r -> unsafeShiftL r 4 + fromIntegral x) 0

mask = (1 `unsafeShiftL` 4) - 1

fromPuzzle :: Integral a => Int -> Puzzle -> [a]
fromPuzzle n p = fp p (n*n)
    where fp p 0 = []
          fp p z = fromIntegral (p .&. mask) : fp (p `unsafeShiftR` 4) (z-1)

{-# INLINE numAt #-}
numAt n p (i,j) = unsafeShiftR p (4*(n*i + j)) .&. mask

{-# INLINE findElem #-}
findElem n p e = flip quotRem n . fromJust . elemIndex e $ p

moves :: Int -> Puzzle -> [(Int, Puzzle)]
moves n p = catMaybes $ swap <$> deltas
    where perm = fromPuzzle n p
          z@(zi,zj) = findElem n perm 0
          deltas = [(zi+1,zj), (zi-1,zj), (zi,zj+1), (zi,zj-1)]
          swap b@(bi,bj)
            | bi < 0 || bi >= n || bj < 0 || bj >= n = Nothing
            | otherwise = Just (fromIntegral num, bitSwap)
              where bitSwap = p `xor`
                              unsafeShiftL num (4*(zi*n+zj)) `xor`
                              unsafeShiftL num (4*(bi*n+bj))
                    num = numAt n p b

solvePuzzle :: Int -> [Int] -> V.Vector Word8 -> V.Vector Word8 -> Maybe [Int]
solvePuzzle n xs fdb cdb = do
    let goal = [0..n*n-1]
    guard $ sort xs == goal
    guard $ parity n xs == parity n goal
    let h = if n < 4
              then manhattanDist n
              else \p -> fromIntegral $ max (pCost fringe fdb p)
                                            (pCost corner cdb p)
    aStar n h (toPuzzle xs) (toPuzzle [0..n*n-1])

parity n p = mod (r + c + p') 2
    where (r,c) = findElem n p 0
          (p',_) = foldr f (0,[]) p
          f x (r,xs) = (r + (length . filter (> x)) xs, x:xs)

manhattanDist n p = sum deltas
    where deltas = zipWith (\(a,b) (c,d) -> abs (a-c) + abs (b-d)) g c
          g = [(r,c) | r <- [0..n-1], c <- [0..n-1]]
          locs = indices [0..n-1] (fromPuzzle n p)
          c = map (`quotRem` n) locs

pCost g db p = db V.! patternID g p

type AStar a = State (S.Set Int, P.MinQueue (Int, [Int], Puzzle)) a

aStar :: Int -> (Puzzle -> Int) -> Puzzle -> Puzzle -> Maybe [Int]
aStar n h start goal = evalState aStar' (S.empty, P.singleton (0,[],start))
    where aStar' = do
            (vis, queue) <- get
            case P.minView queue of
              Nothing -> return Nothing
              (Just ((c,p,x), newQueue)) -> do
                 let newVis = S.insert x vis
                 put (newVis, newQueue)

                 when (h x == 0) (traceShowM x)

                 if x == goal
                   then return $ Just (reverse p)
                   else do unless (x `S.member` vis) $
                             forM_ (moves n x) $ \(m, next) ->
                               unless (next `S.member` newVis) $
                                 let newCost = c + 1 + h next in
                                   (modify . mapSnd) $
                                     P.insert (newCost, m:p, next)
                           aStar'

data GoalPattern = GoalPattern
    { start :: [Int]
    , markers :: [Int]
    }

fringe = GoalPattern
    { start = [0,1,1,3,1,1,1,7,1,1,1,11,12,13,14,15]
    , markers = [0,3,7,11,12,13,14,15]
    }

corner = GoalPattern
    { start = [0,1,1,1,1,1,1,1,8,9,10,1,12,13,14,15]
    , markers = [0,8,9,10,12,13,14,15]
    }

dbSize = factorial 16 `div` factorial 8

patternDB g = bfs 4 g

patternID :: GoalPattern -> Puzzle -> Int
patternID g p = permutationNumber 16 8 nums
    where perm = fromPuzzle 4 p
          nums = indices (markers g) perm

bfs :: Int -> GoalPattern -> V.Vector Word8
bfs n g = runST $ do
    db <- VM.new dbSize
    q <- Q.empty (div dbSize 4)
    cnt <- newSTRef (0 :: Int)

    let pID = patternID g

    Q.insert (toPuzzle (start g)) q

    let bfs' = do
         view <- Q.pop q
         case view of
             Nothing -> return ()
             (Just x) -> do
                 c <- VM.read db (pID x)

                 modifySTRef' cnt (+1)
                 soFar <- readSTRef cnt
                 when (mod soFar (10^5 :: Int) == 0) (traceShowM soFar)

                 forM_ (moves n x) $ \(_, next) -> do
                     let nID = pID next
                     d <- VM.read db nID
                     when (d == 0) $ do
                         VM.write db nID (c+1)
                         Q.insert next q

                 bfs'

    bfs'
    VM.write db (pID (toPuzzle (start g))) 0
    V.freeze db
