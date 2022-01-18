module Types.Beckn.Tracking where

import Beckn.Utils.JSON
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude
import Servant.Client (BaseUrl)

data Tracking = Tracking
  { url :: Maybe BaseUrl,
    status :: Maybe TrackingStatus
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data TrackingStatus = ACTIVE | INACTIVE
  deriving (Generic, Show)

instance ToSchema TrackingStatus where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance FromJSON TrackingStatus where
  parseJSON = genericParseJSON constructorsToLowerOptions

instance ToJSON TrackingStatus where
  toJSON = genericToJSON constructorsToLowerOptions
