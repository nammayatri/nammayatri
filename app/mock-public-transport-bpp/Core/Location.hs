module Core.Location where

import Beckn.Prelude
import Beckn.Types.Core.Migration.Gps
import Beckn.Utils.JSON
import Core.Descriptor

data LocationDetails = LocationDetails
  { id :: Text,
    descriptor :: DescriptorId,
    gps :: Gps,
    -- address :: Maybe Address,
    station_code :: Text
    -- city :: Maybe City,
    -- country :: Maybe Country,
    -- circle :: Maybe Circle,
    -- polygon :: Maybe Text,
    -- _3dspace :: Maybe Text,
    --    time :: Time
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
