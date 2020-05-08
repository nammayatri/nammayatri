module Epass.Types.API.Pass where

import           Epass.Types.Common
import           Epass.Types.Storage.Pass
import           Data.Swagger
import           EulerHS.Prelude

data PassRes =
  PassRes
    { pass :: Pass
    } deriving (Generic, ToJSON, ToSchema)

data UpdatePassReq =
  UpdatePassReq
    { action :: Status
    } deriving (Generic, FromJSON, ToSchema)

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
