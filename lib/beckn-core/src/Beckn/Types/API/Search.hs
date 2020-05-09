module Beckn.Types.API.Search where

import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.Mobility.Intent
import Beckn.Types.Mobility.Service
import Data.Swagger
import EulerHS.Prelude
import Servant.Swagger

data SearchReq = SearchReq
  { _context :: Context,
    _message :: Intent
  }
  deriving (Generic)

instance FromJSON SearchReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

type SearchRes = AckResponse

data OnSearchReq = OnSearchReq
  { _context :: Context,
    _message :: Service
  }
  deriving (Generic)

instance FromJSON OnSearchReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data OnSearchRes = OnSearchRes
  { context :: Context,
    message :: Ack
  }
  deriving (Generic, ToJSON)
