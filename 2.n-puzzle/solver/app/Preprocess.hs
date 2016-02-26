module Main where

import Lib
import Serializer
import System.IO

main = withFile "fringeDB" WriteMode (writeWord8Vector (patternDB fringe))
