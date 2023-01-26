module API
  ( API,
    handler,
  )
where

import qualified API.DistanceMatrix as DistanceMatrix
import qualified API.PlaceName as PlaceName
import qualified API.SnapToRoad as SnapToRoad
import qualified Beckn.External.Maps.Google.MapsClient as Maps
import qualified Beckn.External.Maps.Google.RoadsClient as Roads
import Environment
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
