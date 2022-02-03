module Core.Search.LocationGps where

import Beckn.Types.Core.Migration.Gps
import Data.Aeson
import Relude hiding (id)
import Beckn.Utils.GenericPretty (PrettyShow)

newtype LocationGps = LocationGps {gps :: Gps}
  deriving (Generic, Show, ToJSON, FromJSON, PrettyShow)
