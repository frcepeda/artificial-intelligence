{-# LANGUAGE TypeFamilies #-}

module Lib
    ( solvePuzzle
    , patternDB
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

--fromPuzzle :: Integral a => Int -> Puzzle -> [a]
fromPuzzle n p = fp p (n*n)
    where fp p 0 = []
          fp p z = p .&. mask : fp (p `unsafeShiftR` 4) (z-1)

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

solvePuzzle :: Int -> [Int] -> V.Vector Word8 -> Maybe [Int]
solvePuzzle n xs db = do
    let goal = [0..n*n-1]
    guard $ sort xs == goal
    guard $ parity n xs == parity n goal
    let h = if n < 4 then manhattanDist n else fromIntegral . pCost db
    aStar n h (toPuzzle xs) (toPuzzle [0..n*n-1])

parity n p = mod (r + c + p') 2
    where (r,c) = findElem n p 0
          (p',_) = foldr f (0,[]) p
          f x (r,xs) = (r + (length . filter (> x)) xs, x:xs)

manhattanDist n p = 0
pCost db p = db V.! patternID p

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

patternGoal = [0,1,1,3,1,1,1,7,1,1,1,11,12,13,14,15]
patternDB = bfs 4 (toPuzzle patternGoal)

dbSize = factorial 16 `div` factorial 8

patternID :: Puzzle -> Int
patternID p = permutationNumber 16 8 nums
    where markers = [0,3,7,11,12,13,14,15]
          nums = map
                   (\i -> fromJust . elemIndex i . fromPuzzle 4 $ p)
                   markers

permutationNumber _ _ [] = 0
permutationNumber n k (x:xs) =
    this + permutationNumber (n-1) (k-1) (shiftLess x xs)
    where this = (factorial (n-1) `div` factorial (n-1-(k-1))) * x
          shiftLess x = map (\y -> if y < x then y else y-1)

bfs :: Int -> Puzzle -> V.Vector Word8
bfs n start = runST $ do
    db <- VM.new dbSize
    q <- Q.empty (div dbSize 3)
    cnt <- newSTRef (0 :: Int)
    Q.insert start q
    let bfs' = do
         view <- Q.pop q
         case view of
             Nothing -> return ()
             (Just x) -> do
                 c <- VM.read db (patternID x)

                 modifySTRef' cnt (+1)

                 soFar <- readSTRef cnt
                 when (mod soFar (10^5 :: Int) == 0) (traceShowM soFar)

                 forM_ (moves n x) $ \(_, next) -> do
                     d <- VM.read db (patternID next)
                     when (d == 0) $ do
                         VM.write db (patternID next) (c+1)
                         Q.insert next q

                 --when (soFar < (10^7 :: Int)) bfs'
                 bfs'
    bfs'
    VM.write db (patternID start) 0
    V.freeze db
