module Beckn.Types.API.Location.CRUD where

import           Beckn.Types.App
import qualified Beckn.Types.API.Common as C
import qualified Beckn.Types.Common as C
import           Beckn.Types.Storage.Blacklist
import           Beckn.Types.Storage.EntityTag
import           Beckn.Types.Storage.Location
import           Beckn.Types.Storage.Tag
import           Beckn.Utils.Common
import           Data.Default
import           Data.Time.LocalTime
import           EulerHS.Prelude

data ListLocationRes =
  ListLocationRes
  { _locationInfo :: [C.LocationInfo]
  }
  deriving (Show, Generic)

instance ToJSON ListLocationRes where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON ListLocationRes where
  parseJSON = genericParseJSON stripLensPrefixOptions