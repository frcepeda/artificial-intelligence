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

{-
indices xs ys = runST $ do
    v <- VM.new 16
    let go (p:ps) i = VM.write v p i >> go ps (i+1)
        go [] _ = return ()
    go ys 0
    forM xs (\x -> VM.read v x)

permutationNumber _ _ [] = 0
permutationNumber n k (x:xs) =
    this + permutationNumber (n-1) (k-1) (shiftLess x xs)
    where this = (factorial (n-1) `div` factorial (n-1-(k-1))) * x
          shiftLess x = map (\y -> if y < x then y else y-1)
-}
