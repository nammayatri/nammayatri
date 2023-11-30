{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.CallBPP where

import qualified Beckn.ACL.Track as TrackACL
import qualified Beckn.Types.Core.Metro.API.Search as MigAPI
import Beckn.Types.Core.Taxi.API.Cancel as API
import Beckn.Types.Core.Taxi.API.Confirm as API
import qualified Beckn.Types.Core.Taxi.API.Init as API
import Beckn.Types.Core.Taxi.API.Rating as API
import qualified Beckn.Types.Core.Taxi.API.Search as API
import Beckn.Types.Core.Taxi.API.Select as API
import Beckn.Types.Core.Taxi.API.Status as API
import Beckn.Types.Core.Taxi.API.Track as API
import Beckn.Types.Core.Taxi.API.Update as API
import qualified Data.HashMap as HM
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.Ride as DRide
import qualified EulerHS.Types as Euler
import GHC.Records.Extra
import qualified Kernel.External.Maps.Types as MapSearch
import Kernel.Prelude
import Kernel.Types.Beckn.ReqTypes
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
    CoreMetrics m,
    CacheFlow m r,
    HasField "aclEndPointHashMap" r (HM.Map Text Text)
  ) =>
  BaseUrl ->
  API.SearchReq ->
  m API.SearchRes
search gatewayUrl req = do
  aclEndPointHashMap <- asks (.aclEndPointHashMap)
  callBecknAPIWithSignature req.context.bap_id "search" API.searchAPI gatewayUrl aclEndPointHashMap req

searchMetro ::
  ( MonadFlow m,
    CoreMetrics m,
    CacheFlow m r,
    HasField "aclEndPointHashMap" r (HM.Map Text Text)
  ) =>
  BaseUrl ->
  BecknReq MigAPI.SearchIntent ->
  m ()
searchMetro gatewayUrl req = do
  aclEndPointHashMap <- asks (.aclEndPointHashMap)
  void $ callBecknAPIWithSignatureMetro "search" MigAPI.searchAPI gatewayUrl aclEndPointHashMap req

select ::
  ( MonadFlow m,
    CoreMetrics m,
    CacheFlow m r,
    HasField "aclEndPointHashMap" r (HM.Map Text Text)
  ) =>
  BaseUrl ->
  SelectReq ->
  m SelectRes
select providerUrl req = do
  aclEndPointHashMap <- asks (.aclEndPointHashMap)
  callBecknAPIWithSignature req.context.bap_id "select" API.selectAPI providerUrl aclEndPointHashMap req

init ::
  ( MonadFlow m,
    CoreMetrics m,
    CacheFlow m r,
    HasField "aclEndPointHashMap" r (HM.Map Text Text)
  ) =>
  BaseUrl ->
  API.InitReq ->
  m API.InitRes
init providerUrl req = do
  aclEndPointHashMap <- asks (.aclEndPointHashMap)
  callBecknAPIWithSignature req.context.bap_id "init" API.initAPI providerUrl aclEndPointHashMap req

confirm ::
  ( MonadFlow m,
    CoreMetrics m,
    CacheFlow m r,
    HasField "aclEndPointHashMap" r (HM.Map Text Text)
  ) =>
  BaseUrl ->
  ConfirmReq ->
  m ConfirmRes
confirm providerUrl req = do
  aclEndPointHashMap <- asks (.aclEndPointHashMap)
  callBecknAPIWithSignature req.context.bap_id "confirm" API.confirmAPI providerUrl aclEndPointHashMap req

cancel ::
  ( MonadFlow m,
    CoreMetrics m,
    CacheFlow m r,
    HasField "aclEndPointHashMap" r (HM.Map Text Text)
  ) =>
  BaseUrl ->
  CancelReq ->
  m CancelRes
cancel providerUrl req = do
  aclEndPointHashMap <- asks (.aclEndPointHashMap)
  callBecknAPIWithSignature req.context.bap_id "cancel" API.cancelAPI providerUrl aclEndPointHashMap req

update ::
  ( MonadFlow m,
    CoreMetrics m,
    CacheFlow m r,
    HasField "aclEndPointHashMap" r (HM.Map Text Text)
  ) =>
  BaseUrl ->
  UpdateReq ->
  m UpdateRes
update providerUrl req = do
  aclEndPointHashMap <- asks (.aclEndPointHashMap)
  callBecknAPIWithSignature req.context.bap_id "update" API.updateAPI providerUrl aclEndPointHashMap req

callTrack ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    MonadFlow m,
    CoreMetrics m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasField "aclEndPointHashMap" r (HM.Map Text Text)
  ) =>
  DB.Booking ->
  DRide.Ride ->
  m ()
callTrack booking ride = do
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  bppBookingId <- booking.bppBookingId & fromMaybeM (InvalidRequest "Bpp Booking is missing")
  let merchantOperatingCityId = booking.merchantOperatingCityId
  city <- CQMOC.findById merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
  aclEndPointHashMap <- asks (.aclEndPointHashMap)
  let trackBUildReq =
        TrackACL.TrackBuildReq
          { bppId = booking.providerId,
            bppUrl = booking.providerUrl,
            transactionId = booking.transactionId,
            bppRideId = ride.bppRideId,
            ..
          }
  void . callBecknAPIWithSignature merchant.bapId "track" API.trackAPI booking.providerUrl aclEndPointHashMap =<< TrackACL.buildTrackReq trackBUildReq

data GetLocationRes = GetLocationRes
  { currPoint :: MapSearch.LatLong,
    lastUpdate :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

callGetDriverLocation ::
  ( MonadFlow m,
    CoreMetrics m,
    CacheFlow m r,
    HasField "aclEndPointHashMap" r (HM.Map Text Text)
  ) =>
  Maybe BaseUrl ->
  m GetLocationRes
callGetDriverLocation mTrackingUrl = do
  trackingUrl <- mTrackingUrl & fromMaybeM (RideFieldNotPresent "trackingUrl")
  let eulerClient = Euler.client (Proxy @(Get '[JSON] GetLocationRes))
  aclEndPointHashMap <- asks (.aclEndPointHashMap)
  callApiUnwrappingApiError (identity @TrackUrlError) Nothing (Just "TRACK_URL_NOT_AVAILABLE") (Just aclEndPointHashMap) trackingUrl eulerClient "BPP.driverTrackUrl" (Proxy @(Get '[JSON] GetLocationRes))

feedback ::
  ( MonadFlow m,
    CoreMetrics m,
    CacheFlow m r,
    HasField "aclEndPointHashMap" r (HM.Map Text Text)
  ) =>
  BaseUrl ->
  RatingReqV2 ->
  m RatingRes
feedback providerUrl req = do
  aclEndPointHashMap <- asks (.aclEndPointHashMap)
  callBecknAPIWithSignature req.context.bap_id "feedback" API.ratingAPI providerUrl aclEndPointHashMap req

callStatus ::
  ( MonadFlow m,
    CoreMetrics m,
    CacheFlow m r,
    HasField "aclEndPointHashMap" r (HM.Map Text Text)
  ) =>
  BaseUrl ->
  StatusReq ->
  m StatusRes
callStatus providerUrl req = do
  aclEndPointHashMap <- asks (.aclEndPointHashMap)
  callBecknAPIWithSignature req.context.bap_id "status" API.statusAPI providerUrl aclEndPointHashMap req

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
  HM.Map Text Text ->
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
  HM.Map Text Text ->
  req ->
  m res
callBecknAPIWithSignatureMetro a b c d e = do
  -- bapId <- asks (.bapSelfIds.metro)
  callBecknAPI
    Nothing -- (Just $ Euler.ManagerSelector $ getHttpManagerKey bapId)
    Nothing
    a
    b
    c
    d
    e
