{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Lib

main = runEnv 9592 handler

handler req respond = do
    print req
    respond $ responseLBS status200 [] "ok"
