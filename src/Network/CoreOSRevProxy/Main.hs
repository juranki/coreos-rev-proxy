{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString.Char8 as C8
import Data.IORef (IORef, newIORef, readIORef)
import qualified Data.List as L
import Network (withSocketsDo)
import Network.HTTP.Client (defaultManagerSettings, withManager)
import Network.HTTP.ReverseProxy (ProxyDest (..), WaiProxyResponse (..), defaultOnExc, waiProxyTo)
import Network.HTTP.Types (status404)
import Network.Wai (Request, rawPathInfo, Response, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort)

type PathPrefix = C8.ByteString
type Route = (PathPrefix, ProxyDest)
type RoutingTable = IORef [Route]

resp404 :: Response
resp404 = responseLBS status404 [("Content-Type", "text/plain")] "route not found"

matchRoute :: C8.ByteString -> Route -> Bool
matchRoute path (prefix, _dest) = C8.isPrefixOf prefix path

getDest :: RoutingTable -> Request -> IO WaiProxyResponse
getDest rt req = do
  C8.putStrLn $ rawPathInfo req
  rt' <- readIORef rt
  case L.find (matchRoute $ rawPathInfo req) rt' of
    Just (_prefix, route) ->
        return $ WPRProxyDest route
    Nothing ->
        return $ WPRResponse resp404

main :: IO ()
main =
    let settings = setPort 80 defaultSettings
    in withSocketsDo $ do
      rt <- newIORef [("/LT", ProxyDest "127.0.0.1" 8000)]
      withManager defaultManagerSettings $ \manager ->
          runSettings settings $ waiProxyTo (getDest rt) defaultOnExc manager
