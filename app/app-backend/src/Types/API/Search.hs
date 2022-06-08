module Types.API.Search where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong)
import Data.OpenApi
import Domain.Types.SearchRequest (SearchRequest)
import qualified Tools.JSON as J

data SearchReq = OneWaySearch OneWaySearchReq | RentalSearch RentalSearchReq
  deriving (Generic, Show)

instance ToJSON SearchReq where
  toJSON = genericToJSON J.taggedValueOptions

instance FromJSON SearchReq where
  parseJSON = genericParseJSON J.taggedValueOptions

instance ToSchema SearchReq where
  declareNamedSchema = genericDeclareNamedSchema J.taggedValueSchemaOptions

data OneWaySearchReq = OneWaySearchReq
  { origin :: SearchReqLocation,
    destination :: SearchReqLocation
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data RentalSearchReq = RentalSearchReq
  { origin :: SearchReqLocation,
    startTime :: UTCTime
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
