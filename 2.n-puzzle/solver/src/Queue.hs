module Queue
    ( Queue
    , Queue.empty
    , pop
    , insert
    ) where

import Data.STRef
--import qualified Data.Vector.Generic.Mutable as V
import qualified Data.Vector.Unboxed.Mutable as V
import Debug.Trace
import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST

data Queue s a = Q (STRef s Int) (STRef s Int) (V.MVector s a)

empty n = Q <$> newSTRef 0 <*> newSTRef 0 <*> V.new n

pop (Q sr er v) = do
    s <- readSTRef sr
    e <- readSTRef er
    if s == e
        then return Nothing
        else do
            el <- V.unsafeRead v s
            writeSTRef sr (mod (s+1) (V.length v))
            return (Just el)

insert r (Q sr er v) = do
    s <- readSTRef sr
    e <- readSTRef er
    when (s == (mod (e+1) (V.length v))) (error "queue full.")
    V.unsafeWrite v e r
    writeSTRef er (mod (e+1) (V.length v))
