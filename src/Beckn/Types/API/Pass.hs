module Beckn.Types.API.Pass where

import EulerHS.Prelude
import Beckn.Types.Storage.Pass
import Beckn.Types.API.Common

data PassRes =
  PassRes
    { pass :: Pass
    } deriving (Generic, ToJSON)

data UpdatePassReq =
  UpdatePassReq
    { action :: PassAction
    } deriving (Generic, FromJSON)

data ListPassReq =
  ListPassReq
    { _identifierType :: PassIDType
    , _identifier :: Text
    , _limit :: Int
    , _offset :: Int
    , __type ::  PassType
    } deriving (Generic)

instance FromJSON ListPassReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data ListPassRes =
  ListPassRes
    { passes :: [Pass]
    } deriving (Generic, ToJSON)
