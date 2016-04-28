{-# LANGUAGE BangPatterns #-}

module Utils
    ( factorial
    , mapSnd
    , permutationNumber
    , indices
    , permutationCache
    ) where

import Data.Bits
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Array.Repa as R

factorial :: Int -> Int
factorial n = v V.! n
    where v = V.fromList . take 17 $ scanl (*) 1 [1..]

mapSnd f (a,b) = (a, f b)

indices xs ys = map (\x -> fromJust . elemIndex x $ ys) xs

permutationNumber n k xs = p' n k 0 0 xs
    where p' _ _ _ a [] = a
          p' n k b !a (x:xs) = p' (n-1) (k-1) b' (a + this) xs
            where this = (factorial (n-1) `div` factorial (n-1-(k-1))) * me
                  me = x - permutationCache R.! (R.Z R.:. b R.:. x)
                  b' = b `setBit` x

permutationCache :: R.Array R.U R.DIM2 Int
permutationCache = R.fromListUnboxed (R.Z R.:. 2^16 R.:. 16) elems
    where elems = concatMap f [0..2^16-1]
          f :: Int -> [Int]
          f b = go 0 0
            where go i z
                    | i == 16   = []
                    | otherwise = z : go (i+1) (z + if testBit b i then 1 else 0)

count p = foldl' (\a x -> a + (if p x then 1 else 0)) 0
