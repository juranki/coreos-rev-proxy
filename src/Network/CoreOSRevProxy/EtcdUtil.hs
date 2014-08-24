{-# LANGUAGE OverloadedStrings #-}
module Network.CoreOSRevProxy.EtcdUtil
    ( wait
    , getConf
    ) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson ((.:), (.:?), FromJSON(..), Value(..), decode)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Network.CoreOSRevProxy.Types
import qualified Network.HTTP.Client as HC
import Network.HTTP.ReverseProxy (ProxyDest (..))


data Node = Node { createdIndex  :: Integer
		 , key           :: String
		 , modifiedIndex :: Integer
		 , value         :: Maybe String
		 , dir           :: Maybe Bool
		 , nodes         :: Maybe [Node]
		 } deriving (Show)

data Reply = Reply { action   :: String
		   , node     :: Node
		   , prevNode :: Maybe Node
		   } deriving (Show)

instance FromJSON Node where
  parseJSON (Object v) =
    Node <$>
    (v .: "createdIndex")  <*>
    (v .: "key")           <*>
    (v .: "modifiedIndex") <*>
    (v .:? "value")        <*>
    (v .:? "dir")          <*>
    (v .:? "nodes")

instance FromJSON Reply where
  parseJSON (Object v) =
    Reply <$>
    (v .: "action")  <*>
    (v .: "node")    <*>
    (v .:? "prevNode")

parseReply :: LC8.ByteString -> Reply
parseReply = fromJust . decode

getRoutes :: String -> Reply -> [Route]
getRoutes path =
    map (getRoute path) . fromJust . nodes . node

getRoute :: String -> Node -> Route
getRoute path n =
    let [host, port] = splitOn ":" $ fromJust $ value n
    in ( C8.pack $ drop (length path) $ key n
       , ProxyDest { pdHost = C8.pack host
		   , pdPort = read port
		   }
       )

mkReq :: String -> String -> C8.ByteString -> Maybe Int -> HC.Request
mkReq baseUrl path queryString timeout =
    let req = fromJust $ HC.parseUrl $ baseUrl ++ path
    in req { HC.queryString = queryString
	   , HC.responseTimeout = timeout
	   }

wait :: HC.Manager -> String -> String -> IO ()
wait manager baseUrl path =
    let req = mkReq baseUrl path "wait=true&recursive=true" Nothing
    in do
      _resp <- HC.httpLbs req manager
      return ()

getConf :: HC.Manager -> String -> String -> IO [Route]
getConf  manager baseUrl path =
    let req = mkReq baseUrl path "recursive=true" $ Just 30000000
    in do
      resp <- HC.httpLbs req manager
      return $ getRoutes path $ parseReply $ HC.responseBody resp
