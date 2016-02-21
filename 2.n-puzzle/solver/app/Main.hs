{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Debug.Trace
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.ST
import Data.Aeson
import Data.Word
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic.Mutable as V
import Data.Typeable
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8

import Lib

noSolution = PuzzleSolution Nothing

data PuzzleRequest = PuzzleRequest
    { size :: Int
    , permutation :: [Int]
    , token :: String
    } deriving (Generic, Show)

data PuzzleSolution = PuzzleSolution
        { solution :: Maybe [Int]
        } deriving (Generic, Show)

data ArgException = ArgException
    deriving (Show, Typeable)

instance Exception ArgException
instance FromJSON PuzzleRequest
instance ToJSON PuzzleSolution

main = do
    putStrLn "Loading Pattern Database..."
    db <- decodeWord8Vector <$> B.readFile "patternDB.bak"
    seq db $ putStrLn "Ready!"
    runEnv 9592 (handler db)

handler db req respond = do
    r <- catch (do
                    body <- B.fromStrict <$> requestBody req
                    let (Just puzzle) = decode body
                    unless (token puzzle == "SEEKRITTOKEN") (throw ArgException)
                    return $ solvePuzzle (size puzzle) (permutation puzzle) db)
               (\(e :: ArgException) -> return Nothing)
    respond $ responseLBS status200 origin (encode . PuzzleSolution $ r)
        where origin = [("Access-Control-Allow-Origin", "*")]

decodeWord8Vector :: B.ByteString -> V.Vector Word8
decodeWord8Vector s = runST $ do
    db <- V.new len
    let loop i r = when (i < len) $ do
            V.write db i (B.head r)
            loop (i+1) (B.tail r)
    loop 0 contents
    V.freeze db
    where (lenStr, contents) = B.splitAt 8 s
          len = B.foldl (\a x -> a*2^8 + fromIntegral x) 0 lenStr
