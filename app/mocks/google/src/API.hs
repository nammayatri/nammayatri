module API
  ( API,
    handler,
  )
where

import qualified API.DistanceMatrix as DistanceMatrix
import qualified API.PlaceName as PlaceName
import qualified API.SnapToRoad as SnapToRoad
import Environment
import qualified Kernel.External.Maps.Google.MapsClient as Maps
import qualified Kernel.External.Maps.Google.RoadsClient as Roads
import Servant

type API =
  Maps.DistanceMatrixAPI
    :<|> Maps.PlaceNameAPI
    :<|> Roads.SnapToRoadAPI

handler :: FlowServer API
handler =
  DistanceMatrix.handler
    :<|> PlaceName.handler
    :<|> SnapToRoad.handler
