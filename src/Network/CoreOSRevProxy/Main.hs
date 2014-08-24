{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as C8
import Data.IORef (newIORef, readIORef, atomicWriteIORef)
import qualified Data.List as L
import Network (withSocketsDo)
import qualified Network.HTTP.Client as HC
import Network.HTTP.ReverseProxy (WaiProxyResponse (..), defaultOnExc, waiProxyTo)
import Network.HTTP.Types (status404)
import Network.Wai (Request, rawPathInfo, Response, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort)
import System.Environment (getEnv)
import Network.CoreOSRevProxy.EtcdUtil (wait, getConf)
import Network.CoreOSRevProxy.Types

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
      etcdUrl      <- getEnv "ETCD_URL"
      etcdConfPath <- getEnv "ETCD_PATH"
      rt <- newIORef []
      manager <- HC.newManager HC.defaultManagerSettings
      rtVal0 <- getConf manager etcdUrl etcdConfPath
      atomicWriteIORef rt rtVal0
      _ <- forkIO $ runSettings settings $ waiProxyTo (getDest rt) defaultOnExc manager
      forever $ do
	wait manager etcdUrl etcdConfPath
	rtVal <- getConf manager etcdUrl etcdConfPath
	atomicWriteIORef rt rtVal
