module Utils
    ( mapSnd
    ) where

mapSnd f (a,b) = (a,f b)
