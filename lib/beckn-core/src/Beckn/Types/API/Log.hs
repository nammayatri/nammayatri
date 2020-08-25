module Beckn.Types.API.Log where

import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Utils.Servant.HeaderAuth
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

data LogReq = LogReq
  { _context :: Context,
    _message :: Log
  }
  deriving (Generic, Show)

instance FromJSON LogReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON LogReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

data Log = Log
  { _type :: Text, -- ERROR, WARN, FATAL, DEBUG, INFO
    _message :: Text,
    _errorCode :: Text,
    _debug :: Maybe Text,
    _trace :: Maybe Text,
    _context :: Context
  }
  deriving (Generic, Show)

instance FromJSON Log where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Log where
  toJSON = genericToJSON stripAllLensPrefixOptions

type LogAPI v =
  "log"
    :> APIKeyAuth v
    :> ReqBody '[JSON] LogReq
    :> Post '[JSON] AckResponse

logAPI :: Proxy (LogAPI v)
logAPI = Proxy
