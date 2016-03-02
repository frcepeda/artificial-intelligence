{-# LANGUAGE DeriveGeneric #-}

module Connect4.Types
    ( Experience(..)
    , BoardState
    ) where

import GHC.Generics

data Experience = Novice
                | Amateur
                | Expert
    deriving (Show, Eq, Ord, Generic)
