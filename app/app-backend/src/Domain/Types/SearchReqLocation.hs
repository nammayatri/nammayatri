module Domain.Types.SearchReqLocation where

import Beckn.Prelude
import Beckn.Product.MapSearch.GoogleMaps (HasCoordinates)
import Beckn.Types.Id

data SearchReqLocation = SearchReqLocation
  { id :: Id SearchReqLocation,
    lat :: Double,
    lon :: Double,
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
