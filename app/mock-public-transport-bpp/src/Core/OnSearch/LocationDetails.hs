module Core.OnSearch.LocationDetails where

import Beckn.Prelude
import Core.OnSearch.Descriptor
import Beckn.Types.Core.Migration.Gps
import Beckn.Utils.JSON

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
