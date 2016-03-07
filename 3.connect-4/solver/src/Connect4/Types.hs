{-# LANGUAGE DeriveGeneric #-}

module Connect4.Types
    ( Experience(..)
    , BoardState(..)
    , opponent
    ) where

import GHC.Generics
import Data.Array.Unboxed
import Data.Array

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
    { grid :: Array (Int,Int) (Maybe Player) -- ^ The game grid
    , top :: Array Int Int -- ^ The next available row for every column
    , player :: Player -- ^ whose turn it is
    }
