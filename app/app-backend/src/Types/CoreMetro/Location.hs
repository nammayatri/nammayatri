module Types.CoreMetro.Location where

import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.Gps (Gps)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)

data Location = Location
  { id :: Text,
    descriptor :: Descriptor,
    gps :: Gps,
    station_code :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
