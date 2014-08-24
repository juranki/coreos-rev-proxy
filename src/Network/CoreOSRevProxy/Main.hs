{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Network (withSocketsDo)
import Network.HTTP.Client (defaultManagerSettings, withManager)
import Network.HTTP.ReverseProxy (ProxyDest (..), WaiProxyResponse (..), defaultOnExc, waiProxyTo)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort)
import Network.Wai (Request)
import Network.Wai (rawPathInfo)
import qualified Data.ByteString.Char8 as C8

getDest :: Request -> IO WaiProxyResponse
getDest req = do
  C8.putStrLn $ rawPathInfo req
  return $ WPRProxyDest $ ProxyDest "127.0.0.1" 8000

main :: IO ()
main =
    let settings = setPort 80 defaultSettings
    in withSocketsDo $ do
      withManager defaultManagerSettings $ \manager ->
          runSettings settings $ waiProxyTo getDest defaultOnExc manager

