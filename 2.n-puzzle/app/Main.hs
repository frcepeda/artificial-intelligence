{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Typeable
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import qualified Data.ByteString.Lazy as B

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

main = runEnv 9592 handler

handler req respond = do
    r <- catch (do
                    body <- B.fromStrict <$> requestBody req
                    let (Just puzzle) = decode body
                    unless (token puzzle == "SEEKRITTOKEN") (throw ArgException)
                    return $ solvePuzzle (size puzzle) (permutation puzzle))
               (\(e :: ArgException) -> return Nothing)
    respond $ responseLBS status200 origin (encode . PuzzleSolution $ r)
        where origin = [("Access-Control-Allow-Origin", "*")]
