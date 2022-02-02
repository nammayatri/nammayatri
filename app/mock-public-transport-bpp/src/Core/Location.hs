module Core.Location where

import Beckn.Types.Core.Migration.Gps
import Beckn.Utils.JSON
import Core.Descriptor
import Data.Aeson
import Relude hiding (id)

newtype LocationGps = LocationGps {gps :: Gps}
  deriving (Generic, Show, ToJSON, FromJSON)

data LocationDetails = LocationDetails
  { id :: Text,
    descriptor :: DescriptorId,
    gps :: Gps,
    stop_code :: Text
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
