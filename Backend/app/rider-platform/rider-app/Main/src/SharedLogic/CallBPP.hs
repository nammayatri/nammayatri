{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.CallBPP where

import qualified Beckn.ACL.Metro.Search as MetroACL
import qualified Beckn.ACL.Track as TrackACL
import Beckn.Types.Core.Taxi.API.Cancel as API
import Beckn.Types.Core.Taxi.API.Confirm as API
import qualified Beckn.Types.Core.Taxi.API.Init as API
import Beckn.Types.Core.Taxi.API.Rating as API
import qualified Beckn.Types.Core.Taxi.API.Search as API
import Beckn.Types.Core.Taxi.API.Select as API
import Beckn.Types.Core.Taxi.API.Status as API
import Beckn.Types.Core.Taxi.API.Track as API
import Beckn.Types.Core.Taxi.API.Update as API
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.Ride as DRide
import qualified EulerHS.Types as Euler
import GHC.Records.Extra
import qualified Kernel.External.Maps.Types as MapSearch
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError (IsBecknAPI)
import Kernel.Utils.Monitoring.Prometheus.Servant (SanitizedUrl)
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error
import Tools.Metrics (CoreMetrics)

search ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  API.SearchReq ->
  m API.SearchRes
search gatewayUrl req = do
  callBecknAPIWithSignature req.context.bap_id "search" API.searchAPI gatewayUrl req

searchMetro ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  MetroACL.MetroSearchReq ->
  m ()
searchMetro gatewayUrl req = do
  void $ callBecknAPIWithSignature req.context.bap_id "search" MetroACL.metroSearchAPI gatewayUrl req

select ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  SelectReq ->
  m SelectRes
select providerUrl req = callBecknAPIWithSignature req.context.bap_id "select" API.selectAPI providerUrl req

init ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  API.InitReq ->
  m API.InitRes
init providerUrl req = callBecknAPIWithSignature req.context.bap_id "init" API.initAPI providerUrl req

confirm ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  ConfirmReq ->
  m ConfirmRes
confirm providerUrl req = callBecknAPIWithSignature req.context.bap_id "confirm" API.confirmAPI providerUrl req

cancel ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  CancelReq ->
  m CancelRes
cancel providerUrl req = callBecknAPIWithSignature req.context.bap_id "cancel" API.cancelAPI providerUrl req

update ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  UpdateReq ->
  m UpdateRes
update providerUrl req = callBecknAPIWithSignature req.context.bap_id "update" API.updateAPI providerUrl req

callTrack ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    MonadFlow m,
    CoreMetrics m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  DB.Booking ->
  DRide.Ride ->
  m ()
callTrack booking ride = do
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  bppBookingId <- booking.bppBookingId & fromMaybeM (InvalidRequest "Bpp Booking is missing")
  let merchantOperatingCityId = booking.merchantOperatingCityId
  city <- CQMOC.findById merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
  let trackBUildReq =
        TrackACL.TrackBuildReq
          { bppId = booking.providerId,
            bppUrl = booking.providerUrl,
            transactionId = booking.transactionId,
            bppRideId = ride.bppRideId,
            ..
          }
  void . callBecknAPIWithSignature merchant.bapId "track" API.trackAPI booking.providerUrl =<< TrackACL.buildTrackReq trackBUildReq

data GetLocationRes = GetLocationRes
  { currPoint :: MapSearch.LatLong,
    lastUpdate :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

callGetDriverLocation ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  Maybe BaseUrl ->
  m GetLocationRes
callGetDriverLocation mTrackingUrl = do
  trackingUrl <- mTrackingUrl & fromMaybeM (RideFieldNotPresent "trackingUrl")
  let eulerClient = Euler.client (Proxy @(Get '[JSON] GetLocationRes))
  callApiUnwrappingApiError (identity @TrackUrlError) Nothing (Just "TRACK_URL_NOT_AVAILABLE") trackingUrl eulerClient "BPP.driverTrackUrl" (Proxy @(Get '[JSON] GetLocationRes))

feedback ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  RatingReq ->
  m RatingRes
feedback providerUrl req = callBecknAPIWithSignature req.context.bap_id "feedback" API.ratingAPI providerUrl req

callStatus ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  StatusReq ->
  m StatusRes
callStatus providerUrl req = callBecknAPIWithSignature req.context.bap_id "status" API.statusAPI providerUrl req

callBecknAPIWithSignature ::
  ( MonadFlow m,
    CoreMetrics m,
    IsBecknAPI api req res,
    SanitizedUrl api
  ) =>
  Text ->
  Text ->
  Proxy api ->
  BaseUrl ->
  req ->
  m res
callBecknAPIWithSignature a = callBecknAPI (Just $ Euler.ManagerSelector $ getHttpManagerKey a) Nothing

callBecknAPIWithSignatureMetro ::
  ( MonadFlow m,
    CoreMetrics m,
    IsBecknAPI api req res,
    SanitizedUrl api
  ) =>
  Text ->
  Proxy api ->
  BaseUrl ->
  req ->
  m res
callBecknAPIWithSignatureMetro a b c d = do
  -- bapId <- asks (.bapSelfIds.metro)
  callBecknAPI
    Nothing -- (Just $ Euler.ManagerSelector $ getHttpManagerKey bapId)
    Nothing
    a
    b
    c
    d
