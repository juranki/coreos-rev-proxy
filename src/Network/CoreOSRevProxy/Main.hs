{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Network (withSocketsDo)
import Network.HTTP.Client (defaultManagerSettings, withManager)
import Network.HTTP.ReverseProxy (ProxyDest (..), WaiProxyResponse (..), defaultOnExc, waiProxyTo)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort)

main :: IO ()
main =
    let settings = setPort 80 defaultSettings
    in withSocketsDo $ do
      withManager defaultManagerSettings $ \manager ->
          runSettings settings $ waiProxyTo (const $ return $ WPRProxyDest $ ProxyDest "127.0.0.1" 8000) defaultOnExc manager

