module Beckn.External.Graphhopper.Flow where

import qualified Beckn.External.Graphhopper.Types as GrphrSearch
import Beckn.Types.App (MandatoryQueryParam)
import Data.Geospatial
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
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

search :: BaseUrl -> GrphrSearch.Request -> L.Flow (Either ClientError GrphrSearch.Response)
search url GrphrSearch.Request {..} =
  L.callAPI url clientM
  where
    encodePoint :: PointXY -> Text
    encodePoint point = (show $ _xyX point) <> "," <> (show $ _xyY point)
    points_encoded = False -- Hardcoded `points_encoded` field
    weighting = (T.toLower . show) <$> _weighting
    vehicle = T.toLower $ show _vehicle
    points = encodePoint <$> _points'
    clientM = ET.client grphrAPI points vehicle weighting _elevation points_encoded _calcPoints

defaultGrphrBaseUrl :: BaseUrl
defaultGrphrBaseUrl =
  BaseUrl
    { baseUrlScheme = Https,
      baseUrlHost = "api.sandbox.beckn.juspay.in",
      baseUrlPort = 443,
      baseUrlPath = "/map/grphr"
    }
