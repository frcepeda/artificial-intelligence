module Utils
    ( factorial
    , mapSnd
    ) where

factorial n = product [1..n]

mapSnd f (a,b) = (a, f b)
