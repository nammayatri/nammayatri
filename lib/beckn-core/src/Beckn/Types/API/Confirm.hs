module Beckn.Types.API.Confirm where

import           Beckn.Types.Core.Context
import           Beckn.Types.Core.Ack
import           Beckn.Types.Mobility.Service
import           Data.Swagger
import           EulerHS.Prelude
import           Servant.Swagger

data ConfirmReq =
  ConfirmReq
    { _context :: Context
    , _message :: Service
    }
  deriving (Generic)

instance FromJSON ConfirmReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions


data ConfirmRes =
  ConfirmRes
    { context :: Context
    , message :: Ack
    }
  deriving (Generic, ToJSON)


data OnConfirmReq =
  OnConfirmReq
    { _context :: Context
    , _message :: Service
    }
  deriving (Generic)

instance FromJSON OnConfirmReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions


data OnConfirmRes =
  OnConfirmRes
    { context :: Context
    , message :: Ack
    }
  deriving (Generic, ToJSON)