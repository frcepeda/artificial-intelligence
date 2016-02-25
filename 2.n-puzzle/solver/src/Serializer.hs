module Serializer
   ( writeWord8Vector
   , decodeWord8Vector
   ) where

import System.IO
import Data.Monoid
import Data.Word
import Control.Monad
import Control.Monad.ST
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Builder
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

writeWord8Vector :: V.Vector Word8 -> Handle -> IO ()
writeWord8Vector v h = hPutBuilder h (header <> V.foldr f mempty v)
    where header = word64BE . fromIntegral . V.length $ v
          f i b = word8 i <> b

decodeWord8Vector :: B.ByteString -> V.Vector Word8
decodeWord8Vector s = runST $ do
    db <- VM.new len
    let loop i r = when (i < len) $ do
            VM.write db i (B.head r)
            loop (i+1) (B.tail r)
    loop 0 contents
    V.freeze db
    where (lenStr, contents) = B.splitAt 8 s
          len = B.foldl' (\a x -> a*2^8 + fromIntegral x) 0 lenStr
