{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Connect4
    ( module Connect4.Types
    , bestMove
    ) where

import Control.Applicative
import Connect4.Types
import Data.Array.IArray
import Data.Maybe
import Debug.Trace

bestMove :: BoardState -> Experience -> Maybe Int
bestMove b e
    | isFull b = Nothing
    | otherwise = traceShow (stateScore b) $ Just (negamax b h d)
        where h = heuristic e
              d = depth e

heuristic _ = stateScore
depth _ = 5
scale = [0, 4^0, 4^1, 4^2, inf `div` (4^4)]

inf = 10^6 -- never forget

isFull :: BoardState -> Bool
isFull BoardState{..} = all (== height grid) (elems top)


{-| The heuristic score for a state.
It must satisfy the zero-sum property.
(i.e. stateScore b = - stateScore b' where b
and b' have opposite player fields.)
Also, a winning position must award inf/2
points to the winning player.
-}
stateScore :: BoardState -> Int
stateScore BoardState{..} = sum $ concatMap subs [1..4]
  where r = 6; c = 7 -- FIXME: don't hardcode
        subs n = map score $ h ++ v ++ d1 ++ d2
          where h  = go [0..r-1] [0..c-n]   (\a b d -> (a, b+d))
                v  = go [0..r-n] [0..c-1]   (\a b d -> (a+d, b))
                d1 = go [0..r-n] [0..c-n]   (\a b d -> (a+d, b+d))
                d2 = go [0..r-n] [n-1..c-1] (\a b d -> (a+d, b-d))
                go r c f = [ixmap (0,n-1) (f a b) grid
                           | a <- r, b <- c]
                score b
                  | pl > 0 && op > 0 = 0
                  | pl > 0 = scale !! n
                  | op > 0 = - scale !! n
                  | otherwise = 0
                    where e = elems b
                          pl = count (== Just player) e
                          op = count (== Just (opponent player)) e

{-| List all the possible new board
states possible from the given one.
-}
moves :: BoardState -> [(Int, BoardState)]
moves BoardState{..} = map (\i -> (snd i, move i)) valid
    where valid = filter notFull (assocs top)
          notFull (_,z) = z < height grid
          move (i,z) = BoardState
                         { grid = grid // [((z, i), Just player)]
                         , top = top // [(i, z+1)]
                         , player = opponent player
                         }

negamax :: BoardState -- ^ The starting state
        -> (BoardState -> Int) -- ^ A heuristic
        -> Int -- ^ Maximum allowed depth
        -> Int -- ^ Best move
negamax b h d = snd $ go d b (-inf,inf)
  where go d b (a0, beta)
          | d == 0 || isFull b = (h b, undefined)
          | otherwise = foldAlpha (-inf, undefined) (moves b)
          where foldAlpha az [] = az
                foldAlpha az@(al,_) ~((m,b'):ms)
                  | al > beta = az -- prune by alpha beta
                  | otherwise = foldAlpha (max az bz) ms
                    where (r,_) = go (d-1) b' (-beta,-al)
                          bz = (-r, m)

count :: (a -> Bool) -> [a] -> Int
count p xs = go xs 0
    where go (x:xs) !z = go xs (z + (if p x then 0 else 1))
          go _ z = z
