module Core.OnSearch.LocationDetails where

import Beckn.Prelude
import Beckn.Types.Core.Migration.Gps
import Core.OnSearch.Descriptor

data LocationDetails = LocationDetails
  { id :: Text,
    descriptor :: DescriptorId,
    gps :: Gps,
    stop_code :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)
