module Utils
    ( factorial
    , mapSnd
    ) where

import qualified Data.Vector.Unboxed as V

factorial :: Int -> Int
factorial n = v V.! n
    where v = V.fromList . take 17 $ scanl (*) 1 [1..]

mapSnd f (a,b) = (a, f b)
