module Main where

import Lib
import Serializer
import System.IO

main = withFile "cornerDB" WriteMode (writeWord8Vector (patternDB corner))
