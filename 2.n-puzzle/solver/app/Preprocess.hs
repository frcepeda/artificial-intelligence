module Main where

import Lib
import Serializer
import System.IO

main = withFile "patternDB" WriteMode (writeWord8Vector patternDB)
