module Beckn.Types.API.Pass where

import           Beckn.Types.App
import           Beckn.Types.Common
import           Beckn.Types.Storage.Pass
import           Data.Swagger
import           EulerHS.Prelude

data PassRes =
  PassRes
    { _pass :: Pass
    } deriving (Generic, ToJSON, ToSchema)

data UpdatePassReq =
  UpdatePassReq
    { _action       :: Maybe Status
    , _CustomerId   :: Maybe CustomerId
    , _fromLocation :: Maybe Location
    , _toLocation   :: Maybe Location
    } deriving (Generic, ToSchema)

instance FromJSON UpdatePassReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data ListPassReq =
  ListPassReq
    { _identifierType :: PassIDType
    , _identifier     :: Text
    , _limit          :: Int
    , _offset         :: Int
    , __type          ::  PassType
    } deriving (Generic, ToSchema)

instance FromJSON ListPassReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data ListPassRes =
  ListPassRes
    { passes :: [Pass]
    } deriving (Generic, ToJSON, ToSchema)
