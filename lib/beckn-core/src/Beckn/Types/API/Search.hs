module Beckn.Types.API.Search where

import           Beckn.Types.External.Core.Context
import           Beckn.Types.External.Core.Ack
import           Beckn.Types.External.Mobility.Service
import           Beckn.Types.External.Mobility.Intent
import           Data.Swagger
import           EulerHS.Prelude
import           Servant.Swagger

data SearchReq =
  SearchReq
    { _context :: Context
    , _message :: Intent
    }
  deriving (Generic)

instance FromJSON SearchReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions


data SearchRes =
  SearchRes
    { context :: Context
    , message :: Ack
    }
  deriving (Generic, ToJSON)


data OnSearchReq =
  OnSearchReq
    { _context :: Context
    , _message :: Service
    }
  deriving (Generic)

instance FromJSON OnSearchReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions


data OnSearchRes =
  OnSearchRes
    { context :: Context
    , message :: Ack
    }
  deriving (Generic, ToJSON)