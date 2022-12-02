module Domain.Types.SavedReqLocation where

import Beckn.Prelude
import Beckn.Types.Id
import Domain.Types.Person (Person)

data SavedReqLocation = SavedReqLocation
  { id :: Id SavedReqLocation,
    lat :: Double,
    lon :: Double,
    street :: Maybe Text,
    door :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    tag :: Text,
    riderId :: Id Person,
    placeId :: Maybe Text
  }
  deriving (Generic, Show)

data SavedReqLocationAPIEntity = SavedReqLocationAPIEntity
  { lat :: Double,
    lon :: Double,
    street :: Maybe Text,
    door :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text,
    tag :: Text,
    placeId :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

makeSavedReqLocationAPIEntity :: SavedReqLocation -> SavedReqLocationAPIEntity
makeSavedReqLocationAPIEntity SavedReqLocation {..} =
  SavedReqLocationAPIEntity
    { ..
    }
