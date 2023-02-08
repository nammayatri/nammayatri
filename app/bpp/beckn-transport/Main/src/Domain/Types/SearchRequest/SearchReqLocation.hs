module Domain.Types.SearchRequest.SearchReqLocation where

import Data.Aeson
import Data.OpenApi (ToSchema)
import Data.Time
import EulerHS.Prelude hiding (id, state)
import Kernel.External.Maps.HasCoordinates (HasCoordinates)
import Kernel.Types.Id

data SearchReqLocation = SearchReqLocation
  { id :: Id SearchReqLocation,
    lat :: Double,
    lon :: Double,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq, HasCoordinates)

data SearchReqLocationAPIEntity = SearchReqLocationAPIEntity
  { lat :: Double,
    lon :: Double
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

makeSearchReqLocationAPIEntity :: SearchReqLocation -> SearchReqLocationAPIEntity
makeSearchReqLocationAPIEntity SearchReqLocation {..} =
  SearchReqLocationAPIEntity
    { ..
    }
