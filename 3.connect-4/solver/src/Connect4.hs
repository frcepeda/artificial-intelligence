{-# LANGUAGE RecordWildCards #-}

module Connect4
    ( module Connect4.Types
    , bestMove
    ) where

import Connect4.Types
import Data.Array.IArray

bestMove :: BoardState -> Experience -> Maybe Int
bestMove b e
    | isFull b = Nothing
    | otherwise = Just m
        where (m,_) = negamax b h d
              h = heuristic e
              d = depth e

heuristic _ = undefined
depth _ = undefined

inf = 10^6 -- never forget

isFull :: BoardState -> Bool
isFull BoardState{..} = all (== height) (elems top)
    where height = fst . snd . bounds $ grid
          

{-| The heuristic score for a state.
It must satisfy the zero-sum property.
(i.e. stateScore b = - stateScore b' where b
and b' have opposite player fields.)
Also, a winning position must award inf/2
points to the winning player.
-}
stateScore :: BoardState -> Int
stateScore b = undefined

{-| List all the possible new board
states possible from the given one.
-}
moves :: BoardState -> [(Int, BoardState)]
moves b = undefined

negamax :: BoardState -- ^ The starting state
        -> (BoardState -> Int) -- ^ A heuristic
        -> Int -- ^ Maximum allowed depth
        -> (Int, Int) -- ^ (Best move, score)
negamax b h d = go d b (-inf,inf)
  where go d b (a0,beta)
          | d == 0 || isFull b = (undefined, h b)
          | otherwise = foldAlpha (a0,undefined) (moves b)
          where foldAlpha az@(al,_) ((m,b'):ms)
                  | al > beta = az -- prune by alpha beta
                  | otherwise = foldAlpha (max az bz) ms
                    where (_,r) = go (d-1) b' (-beta,-al)
                          bz = (m, -r)
