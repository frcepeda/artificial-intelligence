{-# LANGUAGE DeriveGeneric #-}

module Connect4.Types
    ( Experience(..)
    , BoardState(..)
    , opponent
    , height
    ) where

import GHC.Generics
import qualified Data.Array.Unboxed as U
import qualified Data.Array as A

data Experience = Novice
                | Amateur
                | Expert
    deriving (Show, Eq, Ord, Generic)

data Player = A | B
    deriving (Show, Eq)

opponent :: Player -> Player
opponent A = B
opponent B = A

data BoardState = BoardState
    { grid :: A.Array (Int,Int) (Maybe Player) -- ^ The game grid
    , top :: U.UArray Int Int -- ^ The next available row for every column
    , player :: Player -- ^ whose turn it is
    }

height :: A.Array (Int,Int) (Maybe Player) -> Int
height = (+1) . fst . snd . A.bounds
