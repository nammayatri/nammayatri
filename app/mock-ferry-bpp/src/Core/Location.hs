module Core.Location where

import Beckn.Prelude
import Beckn.Types.Core.Migration.Gps
import Beckn.Utils.JSON
import Core.Descriptor

newtype LocationGps = LocationGps {gps :: Gps}
  deriving (Generic, Show, ToJSON, FromJSON)

data LocationDetails = LocationDetails
  { id :: Text,
    descriptor :: DescriptorId,
    gps :: Gps,
    station_code :: Text
  }
  deriving (Generic, Show)

instance FromJSON LocationDetails where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON LocationDetails where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

newtype LocationId = LocationId
  { id :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)
