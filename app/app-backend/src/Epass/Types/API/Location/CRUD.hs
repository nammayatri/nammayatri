module Epass.Types.API.Location.CRUD where

import Data.Default
import Data.Time.LocalTime
import qualified Epass.Types.API.Common as C
import Epass.Types.App
import qualified Epass.Types.Common as C
import Epass.Types.Storage.Blacklist
import Epass.Types.Storage.EntityTag
import Epass.Types.Storage.Location
import Epass.Types.Storage.Tag
import EulerHS.Prelude

data ListLocationRes = ListLocationRes
  { _locationInfo :: [C.LocationInfo]
  }
  deriving (Show, Generic)

instance ToJSON ListLocationRes where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON ListLocationRes where
  parseJSON = genericParseJSON stripLensPrefixOptions
