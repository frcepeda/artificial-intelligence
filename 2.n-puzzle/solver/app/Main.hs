{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Debug.Trace
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Word
import Data.Typeable
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import qualified Data.ByteString.Lazy as B

import Lib
import Serializer

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
    let settings = (setPort 9592 . setTimeout 600) defaultSettings
    runSettings settings (handler db)

handler db req respond = do
    r <- catch (do
        body <- B.fromStrict <$> requestBody req
        let (Just puzzle) = decode body
        unless (token puzzle == "SEEKRITTOKEN") (throw ArgException)
        return $ solvePuzzle (size puzzle) (permutation puzzle) db)
               (\(e :: ArgException) -> return Nothing)
    respond $ responseLBS status200 origin (encode . PuzzleSolution $ r)
        where origin = [("Access-Control-Allow-Origin", "*")]
