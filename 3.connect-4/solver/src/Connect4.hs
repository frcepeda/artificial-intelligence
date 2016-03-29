{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Connect4
    ( module Connect4.Types
    , bestMove
    ) where

import Control.Applicative
import Control.Monad
import Connect4.Types
import Data.Monoid
import Data.Array.IArray
import Data.Maybe
import Debug.Trace

bestMove :: BoardState -> Experience -> Maybe Int
bestMove b e
    | isOver b = Nothing
    | otherwise = Just (negamax h d b)
        where h = heuristic e
              d = depth e

heuristic _ = stateScore
depth Novice = 2
depth Amateur = 3
depth Expert = 4
scale = [0, 4^0, 4^3, 4^6, 4^14]

inf = 2^30

isFull :: BoardState -> Bool
isFull BoardState{..} = all (== height grid) (elems top)


{-| The heuristic score for a state.
-}
stateScore :: BoardState -> Int
stateScore b@BoardState{..}
    | Just w <- winner b = if w == player then inf else -inf
    | otherwise = sum . zipWith (*) [7, 7, 4, 4]
                      . map (sum . map score)
                      . subs grid $ 4
  where score b
          | pl == 3 && op == 0 = 1
          | op == 3 && pl == 0 = -1
          | otherwise = 0
            where e = elems b
                  pl = count (== Just player) e
                  op = count (== Just (opponent player)) e

subs grid n = [h, v, d1, d2]
  where h  = go [0..r-1] [0..c-n]   (\a b d -> (a, b+d))
        v  = go [0..r-n] [0..c-1]   (\a b d -> (a+d, b))
        d1 = go [0..r-n] [0..c-n]   (\a b d -> (a+d, b+d))
        d2 = go [0..r-n] [n-1..c-1] (\a b d -> (a+d, b-d))
        go r c f = [ixmap (0,n-1) (f a b) grid
                   | a <- r, b <- c]
        r = 6; c = 7 -- FIXME: don't hardcode

isOver b = isFull b || isJust (winner b)

winner BoardState{..} = getFirst . mconcat . map toWinner $ concat (subs grid 4)
    where toWinner g = First $ do
            let gs = elems g
            a <- head gs
            guard $ all (== Just a) gs
            return a

{-| List all the possible new board
states possible from the given one.
-}
moves :: BoardState -> [(Int, BoardState)]
moves BoardState{..} = map (\i -> (fst i, move i)) valid
    where valid = filter notFull (assocs top)
          notFull (_,z) = z < height grid
          move (i,z) = BoardState
                         { grid = grid // [((z, i), Just player)]
                         , top = top // [(i, z+1)]
                         , player = opponent player
                         }

negamax :: (BoardState -> Int) -- ^ A heuristic
        -> Int -- ^ Search depth
        -> BoardState -- ^ The starting state
        -> Int -- ^ Best move
negamax h d0 b0 = snd $ go d0 b0 (-inf,inf)
  where go d b (a0, beta)
          | d == 0 || isOver b = (h b, -1)
          | otherwise = foldAlpha a0 (-inf,-1) (moves b)
          where foldAlpha al best [] = best
                foldAlpha al best ~((m,b'):ms)
                  | al > beta = best -- prune by alpha beta
                  | otherwise = foldAlpha (max al (-r)) (max best bz) ms
                    where (r,_) = go (d-1) b' (-beta,-al)
                          bz = (-r, m)

count :: (a -> Bool) -> [a] -> Int
count p xs = go xs 0
    where go (x:xs) !z = go xs (z + (if p x then 1 else 0))
          go _ z = z
