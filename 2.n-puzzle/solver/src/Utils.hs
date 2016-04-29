{-# LANGUAGE BangPatterns #-}

module Utils
    ( factorial
    , mapSnd
    , permutationNumber
    , indices
    ) where

import Data.Bits
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

permutationNumber :: Int -> Int -> [Int] -> Int
permutationNumber n k xs = p' n k (0 :: Int) 0 xs
    where p' _ _ _ a [] = a
          p' n k b !a (x:xs) = p' (n-1) (k-1) b' (a + this) xs
            where this = (factorial (n-1) `div` factorial (n-1-(k-1))) * me
                  me = x - popCount (b .&. mask)
                  mask = (1 `unsafeShiftL` x) - 1
                  b' = b `setBit` x

count p = foldl' (\a x -> a + (if p x then 1 else 0)) 0
