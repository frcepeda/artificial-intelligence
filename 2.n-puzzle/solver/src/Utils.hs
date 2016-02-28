{-# LANGUAGE BangPatterns #-}

module Utils
    ( factorial
    , mapSnd
    , permutationNumber
    , indices
    ) where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

factorial :: Int -> Int
factorial n = v V.! n
    where v = V.fromList . take 17 $ scanl (*) 1 [1..]

mapSnd f (a,b) = (a, f b)

indices xs ys = map (\x -> fromJust . elemIndex x $ ys) xs

permutationNumber n k xs = p' n k [] 0 xs
    where p' _ _ _ a [] = a
          p' n k prev !a (x:xs) = p' (n-1) (k-1) (x:prev) (a + this) xs
            where this = (factorial (n-1) `div` factorial (n-1-(k-1))) * me
                  me = x - count (< x) prev

count p = foldl' (\a x -> a + (if p x then 1 else 0)) 0
