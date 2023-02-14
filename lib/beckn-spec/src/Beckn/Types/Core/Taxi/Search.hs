module Beckn.Types.Core.Taxi.Search
  ( module Beckn.Types.Core.Taxi.Search,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Search.Fulfillment as Reexport
import Beckn.Types.Core.Taxi.Search.Intent as Reexport
import Beckn.Types.Core.Taxi.Search.Location as Reexport
import Beckn.Types.Core.Taxi.Search.StartInfo as Reexport
import Beckn.Types.Core.Taxi.Search.StopInfo as Reexport
import Data.Aeson
import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import Kernel.External.Maps (RouteInfo)

data SearchMessage = SearchMessage
  { intent :: Intent,
    routeInfo :: Maybe RouteInfo
  }
  deriving (Generic, Show, ToSchema)

instance ToJSON SearchMessage where
  toJSON = genericToJSON searchMessageJSONOptions

instance FromJSON SearchMessage where
  parseJSON = genericParseJSON searchMessageJSONOptions

searchMessageJSONOptions :: Options
searchMessageJSONOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "routeInfo" -> "./nammayatri/routeInfo"
        a -> a
    }
