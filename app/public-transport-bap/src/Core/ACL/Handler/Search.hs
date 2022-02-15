module Core.ACL.Handler.Search where

import API.Types.Common as Gps
import API.UI.Search.Types
import Beckn.Prelude
import qualified Beckn.Types.Core.Migration.Context as Context
import Beckn.Types.Core.Migration.Gps as BecknGps
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Error
import Beckn.Types.Monitoring.Prometheus.Metrics
import Beckn.Utils.Common
import Core.Spec.Common.Time as Time
import qualified Core.Spec.Search.Intent as Intent
import qualified Data.Text as T
import ExternalAPI.Flow as ExternalAPI
import Text.Read
import Tools.Context
import qualified Types.Domain.Outgoing.Search as DSearch

searchHandler ::
  ( MonadFlow m,
    MonadReader r m,
    CoreMetrics m,
    HasField "config" r c,
    HasField "gatewayUrl" c BaseUrl,
    HasField "selfId" c Text
  ) =>
  SearchReq ->
  BecknGps.Gps ->
  Text ->
  BaseUrl ->
  Maybe BaseUrl ->
  Text ->
  m ()
searchHandler req gps txnId bapURI bppURI bapId = do
  context <- buildContext Context.SEARCH txnId bapURI bppURI bapId
  let intent = mkIntent req gps
  ExternalAPI.search (BecknReq context $ DSearch.SearchIntent intent)

makeGps :: MonadFlow m => Gps.Gps -> m BecknGps.Gps
makeGps location = do
  lat <- readMaybe (T.unpack location.lat) & fromMaybeM (InternalError "Unable to parse lat")
  lon <- readMaybe (T.unpack location.lon) & fromMaybeM (InternalError "Unable to parse lon")
  pure BecknGps.Gps {..}

mkIntent :: SearchReq -> BecknGps.Gps -> Intent.Intent
mkIntent req gps = do
  Intent.Intent
    { fulfillment =
        Intent.FulFillmentInfo
          { start =
              Intent.LocationAndTime
                { location =
                    Intent.Location
                      { gps = gps
                      },
                  time =
                    Time.Time
                      { timestamp = req.toDate
                      }
                },
            end =
              Intent.Location
                { gps = gps
                }
          }
    }
