module Beckn.Types.Core.Taxi.OnTrack.Tracking
  ( module Beckn.Types.Core.Taxi.OnTrack.Tracking,
  )
where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.Aeson as A
import Data.OpenApi (ToSchema (..), fromAesonOptions)

data Tracking = Tracking
  { url :: BaseUrl,
    content_type :: Text
  }
  deriving (Generic, Show)

instance ToSchema Tracking where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions trackingJSONOptions

instance FromJSON Tracking where
  parseJSON = genericParseJSON trackingJSONOptions

instance ToJSON Tracking where
  toJSON = genericToJSON trackingJSONOptions

trackingJSONOptions :: A.Options
trackingJSONOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "content_type" -> "./komn/content-type"
        a -> a
    }
