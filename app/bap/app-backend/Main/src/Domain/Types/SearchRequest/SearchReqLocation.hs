module Domain.Types.SearchRequest.SearchReqLocation where

import Domain.Types.LocationAddress
import Kernel.External.Maps.HasCoordinates
import Kernel.Prelude
import Kernel.Types.Id

data SearchReqLocation = SearchReqLocation
  { id :: Id SearchReqLocation,
    lat :: Double,
    lon :: Double,
    address :: LocationAddress,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, HasCoordinates)

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
