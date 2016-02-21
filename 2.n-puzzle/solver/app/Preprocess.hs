module Main where

import Lib
import Data.Serialize
import Data.Vector.Serialize
import qualified Data.ByteString.Char8 as B

main = do
    let v = patternDB
    B.writeFile "patternDB" $ encode patternDB
