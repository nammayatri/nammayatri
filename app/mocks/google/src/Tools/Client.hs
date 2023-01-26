module Tools.Client where

import qualified Beckn.External.Maps.Google.RoadsClient as Roads
import Beckn.Prelude
import Beckn.Tools.Metrics.CoreMetrics as Metrics
import Beckn.Types.Error (GenericError (InternalError))
import Beckn.Utils.Common (MonadFlow, callAPI, fromEitherM)
import qualified Domain.Types.MockPlace as DPlace
import EulerHS.Types as Euler

snapToRoad ::
  ( HasCallStack,
    Metrics.CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  Text ->
  Text ->
  m DPlace.SnapToRoadResponse
snapToRoad roadsUrl apiKey path = do
  let eulerClient = Euler.client (Proxy @Roads.SnapToRoadAPI)
      interpolate = True
  callAPI roadsUrl (eulerClient apiKey interpolate path) "snap-to-road"
    >>= fromEitherM (\err -> InternalError $ "Failed to call snap-to-road API: " <> show err)
