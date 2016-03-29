{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveDataTypeable  #-}

module Main where

import Debug.Trace
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

import qualified Connect4 as C4

data MoveRequest = MoveRequest
    { target :: Int
    , rows :: Int
    , cols :: Int
    , grid :: [[Maybe Int]]
    , player :: Int
    , experience :: C4.Experience
    , token :: String
    } deriving (Generic, Show)

data MoveResponse = MoveResponse
        { move :: Maybe Int
        } deriving (Generic, Show)

data ArgException = ArgException
    deriving (Show, Typeable)

instance Exception ArgException

instance FromJSON MoveRequest
instance FromJSON C4.Experience
instance ToJSON MoveResponse

main = do
    let settings = (setPort 9593 . setTimeout 600) defaultSettings
    runSettings settings handler

handler req respond = do
    r <- catch (do
        body <- B.fromStrict <$> requestBody req

        let state' = eitherDecode body
        let Right state = state'

        unless (token state == "SEEKRITTOKEN") (throw ArgException)

        let board = C4.buildBoard (rows state, cols state)
                                  (player state)
                                  (grid state)

        return $ C4.bestMove board (experience state)
        )
               (\(e :: ArgException) -> return Nothing)
    respond $ responseLBS status200 origin (encode . MoveResponse $ r)
        where origin = [("Access-Control-Allow-Origin", "*")]
