{-# LANGUAGE DeriveGeneric #-}

module Connect4.Types
    ( Experience(..)
    , BoardState(..)
    , opponent
    , height
    , buildBoard
    ) where

import GHC.Generics
import qualified Data.Array.Unboxed as U
import qualified Data.Array as A
import Data.Maybe

data Experience = Novice
                | Amateur
                | Expert
    deriving (Show, Eq, Ord, Generic)

data Player = A | B
    deriving (Show, Eq)

toPlayer 0 = A
toPlayer 1 = B
toPlayer n = error $ "toPlayer: invalid player. (" ++ show n ++ ")"

opponent :: Player -> Player
opponent A = B
opponent B = A

data BoardState = BoardState
    { grid :: A.Array (Int,Int) (Maybe Player) -- ^ The game grid
    , top :: U.UArray Int Int -- ^ The next available row for every column
    , player :: Player -- ^ whose turn it is
    } deriving (Show)

buildBoard :: (Int,Int) -> Int -> [[Maybe Int]] -> BoardState
buildBoard (r,c) p xss = BoardState
    { grid = g
    , top = U.listArray (0,c-1) (map topr [0..c-1])
    , player = toPlayer p
    } where g = A.listArray ((0,0),(r-1,c-1))
                            (concat . fmap (fmap (fmap toPlayer)) $ xss)
            topr b = go 0
                where go a
                       | a == r || isNothing (g A.! (a,b)) = a
                       | otherwise = go (a+1)

height :: A.Array (Int,Int) (Maybe Player) -> Int
height = (+1) . fst . snd . A.bounds
