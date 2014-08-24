module Network.CoreOSRevProxy.Types
    ( PathPrefix
    , Route
    , RoutingTable
    ) where

import qualified Data.ByteString.Char8 as C8
import Network.HTTP.ReverseProxy (ProxyDest)
import Data.IORef (IORef)

type PathPrefix = C8.ByteString
type Route = (PathPrefix, ProxyDest)
type RoutingTable = IORef [Route]

