module Beckn.External.Graphhopper.Flow where

import qualified Beckn.External.Graphhopper.Types as GrphrSearch
import Beckn.Types.App (MandatoryQueryParam)
import Beckn.Types.Common
import Beckn.Types.Monitoring.Prometheus.Metrics (RequestLatencyMetric)
import Beckn.Utils.Common (callAPI)
import Data.Geospatial
import qualified Data.Text as T
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import GHC.Records
import Servant
import Servant.Client

type GrphrAPI =
  "route"
    :> QueryParams "point" Text
    :> MandatoryQueryParam "vehicle" Text
    :> QueryParam "weighting" Text
    :> QueryParam "elevation" Bool
    :> MandatoryQueryParam "points_encoded" Bool
    :> QueryParam "calc_points" Bool
    :> Get '[JSON] GrphrSearch.Response

grphrAPI :: Proxy GrphrAPI
grphrAPI = Proxy

search :: HasField "metricsRequestLatency" r RequestLatencyMetric => BaseUrl -> GrphrSearch.Request -> FlowR r (Either ClientError GrphrSearch.Response)
search url GrphrSearch.Request {..} =
  callAPI url clientM "search"
  where
    encodePoint :: PointXY -> Text
    encodePoint point = show (_xyX point) <> "," <> show (_xyY point)
    points_encoded = False -- Hardcoded `points_encoded` field
    weighting = T.toLower . show <$> _weighting
    vehicle = T.toLower $ show _vehicle
    points = encodePoint <$> _points'
    clientM = ET.client grphrAPI points vehicle weighting _elevation points_encoded _calcPoints
