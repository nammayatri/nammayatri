{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnTrack.Tracking
  ( module Beckn.Types.Core.Taxi.OnTrack.Tracking,
  )
where

import Data.Aeson
import Data.Aeson.Types
import Data.OpenApi (ToSchema (..), fromAesonOptions)
import Data.Text
import Kernel.Prelude
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Tracking = Tracking
  { url :: BaseUrl,
    -- content_type :: Text,
    status :: TrackingStatus
  }
  deriving (Generic, Show)

instance ToSchema Tracking where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions trackingJSONOptions

instance FromJSON Tracking where
  parseJSON = genericParseJSON trackingJSONOptions

instance ToJSON Tracking where
  toJSON = genericToJSON trackingJSONOptions

trackingJSONOptions :: Options
trackingJSONOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "content_type" -> "content_type"
        a -> a
    }

data TrackingStatus = ACTIVE | INACTIVE
  deriving (Show, Generic, ToSchema)

instance FromJSON TrackingStatus where
  parseJSON (String "ACTIVE") = pure ACTIVE -------this won't happen ideally but just for safer side
  parseJSON (String "INACTIVE") = pure INACTIVE -------this won't happen ideally but just for safer side
  parseJSON (String "active") = pure ACTIVE
  parseJSON (String "inactive") = pure INACTIVE
  parseJSON (String _) = parseFail "Expected \"ACTIVE OR INACTIVE\""
  parseJSON e = typeMismatch "String" e

instance ToJSON TrackingStatus where
  toJSON = String . toLower . show
