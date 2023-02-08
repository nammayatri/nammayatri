module Domain.Types.SearchRequest.SearchReqLocation where

import Kernel.External.Maps.HasCoordinates (HasCoordinates)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.GenericPretty (PrettyShow)

data SearchReqLocation = SearchReqLocation
  { id :: Id SearchReqLocation,
    lat :: Double,
    lon :: Double,
    street :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text,
    full_address :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show, PrettyShow, HasCoordinates)

data SearchReqLocationAPIEntity = SearchReqLocationAPIEntity
  { lat :: Double,
    lon :: Double,
    street :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text,
    full_address :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

makeSearchReqLocationAPIEntity :: SearchReqLocation -> SearchReqLocationAPIEntity
makeSearchReqLocationAPIEntity SearchReqLocation {..} =
  SearchReqLocationAPIEntity
    { ..
    }
