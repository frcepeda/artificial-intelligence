{-# LANGUAGE DeriveGeneric #-}

module Connect4.Types
    ( Experience(..)
    , BoardState(..)
    ) where

import GHC.Generics
import Data.Array.Unboxed

data Experience = Novice
                | Amateur
                | Expert
    deriving (Show, Eq, Ord, Generic)

data Player = A | B
    deriving (Show, Eq)

data BoardState = BoardState
    { grid :: UArray (Int,Int) (Maybe Player) -- ^ The game grid
    , top :: UArray Int Int -- ^ The next available row for every column
    , player :: Player -- ^ whose turn it is
    }
