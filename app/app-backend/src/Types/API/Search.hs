module Types.API.Search where

import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong)
import Data.OpenApi (ToSchema)
import Domain.Types.SearchRequest (SearchRequest)
import EulerHS.Prelude hiding (id, state)

data SearchReq = SearchReq
  { origin :: SearchReqLocation,
    destination :: SearchReqLocation
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data SearchReqLocation = SearchReqLocation
  { address :: SearchReqAddress,
    gps :: LatLong
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data SearchReqAddress = SearchReqAddress
  { door :: Maybe Text,
    building :: Maybe Text,
    street :: Maybe Text,
    area :: Maybe Text,
    city :: Maybe Text,
    country :: Maybe Text,
    areaCode :: Maybe Text,
    state :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype SearchRes = SearchRes
  { searchId :: Id SearchRequest
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
