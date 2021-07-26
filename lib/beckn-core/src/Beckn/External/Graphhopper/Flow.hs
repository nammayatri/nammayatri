module Beckn.External.Graphhopper.Flow where

import qualified Beckn.External.Graphhopper.Types as GrphrSearch
import Beckn.Types.Common
import Beckn.Types.Monitoring.Prometheus.Metrics (CoreMetrics)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Servant
import Servant.Client

type GrphrAPI =
  "route"
    :> ReqBody '[JSON] GrphrSearch.Request
    :> Post '[JSON] GrphrSearch.Response

grphrAPI :: Proxy GrphrAPI
grphrAPI = Proxy

search ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  BaseUrl ->
  GrphrSearch.Request ->
  m (Either ClientError GrphrSearch.Response)
search url = L.callAPI url . ET.client grphrAPI
