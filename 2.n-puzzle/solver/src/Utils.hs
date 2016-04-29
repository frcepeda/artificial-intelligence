{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}

module Utils
    ( factorial
    , mapSnd
    , permutationNumber
    , indices
    ) where

import Foreign
import Foreign.Ptr
import Foreign.Marshal.Utils
import Data.Bits
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.ST
import System.IO.Unsafe
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

foreign import ccall "permutation.h pnum"
    c_pnum :: Ptr Int -> IO Int

factorial :: Int -> Int
factorial n = v V.! n
    where v = V.fromList . take 17 $ scanl (*) 1 [1..]

mapSnd f (a,b) = (a, f b)

indices xs ys = map (\x -> fromJust . elemIndex x $ ys) xs

buffer :: Ptr Int
buffer = unsafePerformIO $ mallocArray 8

permutationNumber :: [Int] -> Int
permutationNumber xs = unsafePerformIO $ do
    pokeArray buffer xs
    c_pnum buffer
