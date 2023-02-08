module Tools.Client where

import qualified Domain.Types.MockPlace as DPlace
import EulerHS.Types as Euler
import qualified Kernel.External.Maps.Google.RoadsClient as Roads
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common (MonadFlow, callAPI, fromEitherM)

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
