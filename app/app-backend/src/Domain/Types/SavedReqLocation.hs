module Domain.Types.SavedReqLocation where

import Beckn.Prelude
import Beckn.Types.Id
import Domain.Types.Person (Person)
import Domain.Types.SearchReqLocation (SearchReqLocationAPIEntity (..))

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
    riderId :: Id Person
  }
  deriving (Generic, Show)

data SavedReqLocationAPIEntity = SavedReqLocationAPIEntity
  { address :: SearchReqLocationAPIEntity,
    tag :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

makeSavedReqLocationAPIEntity :: SavedReqLocation -> SavedReqLocationAPIEntity
makeSavedReqLocationAPIEntity SavedReqLocation {..} =
  let address = SearchReqLocationAPIEntity {..}
   in SavedReqLocationAPIEntity
        { ..
        }
