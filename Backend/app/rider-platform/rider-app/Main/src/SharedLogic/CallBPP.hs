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
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl]
  ) =>
  BaseUrl ->
  API.SearchReq ->
  m API.SearchRes
search gatewayUrl req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  callBecknAPIWithSignature req.context.bap_id "search" API.searchAPIV1 gatewayUrl internalEndPointHashMap req

searchV2 ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl]
  ) =>
  BaseUrl ->
  API.SearchReqV2 ->
  m API.SearchRes
searchV2 gatewayUrl req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  bapId <- req.searchReqContext.contextBapId & fromMaybeM (InvalidRequest "BapId is missing")
  callBecknAPIWithSignature bapId "search" API.searchAPIV2 gatewayUrl internalEndPointHashMap req

searchMetro ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl]
  ) =>
  BaseUrl ->
  BecknReq MigAPI.SearchIntent ->
  m ()
searchMetro gatewayUrl req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  void $ callBecknAPIWithSignatureMetro "search" MigAPI.searchAPI gatewayUrl internalEndPointHashMap req

select ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl]
  ) =>
  BaseUrl ->
  SelectReq ->
  m SelectRes
select providerUrl req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  callBecknAPIWithSignature req.context.bap_id "select" API.selectAPIV1 providerUrl internalEndPointHashMap req

-- selectV2 ::
--   ( MonadFlow m,
--     CoreMetrics m,
--     HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl]
--   ) =>
--   BaseUrl ->
--   SelectReqV2 ->
--   m SelectRes
-- selectV2 providerUrl req = do
--   internalEndPointHashMap <- asks (.internalEndPointHashMap)
--   callBecknAPIWithSignature req.context.bap_id "select" API.selectAPIV2 providerUrl internalEndPointHashMap req

init ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl]
  ) =>
  BaseUrl ->
  API.InitReq ->
  m API.InitRes
init providerUrl req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  callBecknAPIWithSignature req.context.bap_id "init" API.initAPIV1 providerUrl internalEndPointHashMap req

-- initV2 ::
--   ( MonadFlow m,
--     CoreMetrics m,
--     HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl]
--   ) =>
--   BaseUrl ->
--   API.InitReqV2 ->
--   m API.InitRes
-- initV2 providerUrl req = do
--   internalEndPointHashMap <- asks (.internalEndPointHashMap)
--   callBecknAPIWithSignature req.context.bap_id "init" API.initAPIV2 providerUrl internalEndPointHashMap req

confirm ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl]
  ) =>
  BaseUrl ->
  ConfirmReq ->
  m ConfirmRes
confirm providerUrl req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  callBecknAPIWithSignature req.context.bap_id "confirm" API.confirmAPIV1 providerUrl internalEndPointHashMap req

-- confirmV2 ::
--   ( MonadFlow m,
--     CoreMetrics m,
--     HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl]
--   ) =>
--   BaseUrl ->
--   ConfirmReqV2 ->
--   m ConfirmRes
-- confirmV2 providerUrl req = do
--   internalEndPointHashMap <- asks (.internalEndPointHashMap)
--   callBecknAPIWithSignature req.context.bap_id "confirm" API.confirmAPIV2 providerUrl internalEndPointHashMap req

cancel ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl]
  ) =>
  BaseUrl ->
  CancelReq ->
  m CancelRes
cancel providerUrl req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  callBecknAPIWithSignature req.context.bap_id "cancel" API.cancelAPI providerUrl internalEndPointHashMap req

update ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl]
  ) =>
  BaseUrl ->
  UpdateReq ->
  m UpdateRes
update providerUrl req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  callBecknAPIWithSignature req.context.bap_id "update" API.updateAPIV1 providerUrl internalEndPointHashMap req

-- updateV2 ::
--   ( MonadFlow m,
--     CoreMetrics m,
--     HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl]
--   ) =>
--   BaseUrl ->
--   UpdateReqV2 ->
--   m UpdateRes
-- updateV2 providerUrl req = do
--   internalEndPointHashMap <- asks (.internalEndPointHashMap)
--   callBecknAPIWithSignature req.context.bap_id "update" API.updateAPIV2 providerUrl internalEndPointHashMap req

callTrack ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    MonadFlow m,
    CoreMetrics m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl]
  ) =>
  DB.Booking ->
  DRide.Ride ->
  m ()
callTrack booking ride = do
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  bppBookingId <- booking.bppBookingId & fromMaybeM (InvalidRequest "Bpp Booking is missing")
  let merchantOperatingCityId = booking.merchantOperatingCityId
  city <- CQMOC.findById merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  let trackBUildReq =
        TrackACL.TrackBuildReq
          { bppId = booking.providerId,
            bppUrl = booking.providerUrl,
            transactionId = booking.transactionId,
            bppRideId = ride.bppRideId,
            ..
          }
  void . callBecknAPIWithSignature merchant.bapId "track" API.trackAPI booking.providerUrl internalEndPointHashMap =<< TrackACL.buildTrackReq trackBUildReq

data GetLocationRes = GetLocationRes
  { currPoint :: MapSearch.LatLong,
    lastUpdate :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

callGetDriverLocation ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl]
  ) =>
  Maybe BaseUrl ->
  m GetLocationRes
callGetDriverLocation mTrackingUrl = do
  trackingUrl <- mTrackingUrl & fromMaybeM (RideFieldNotPresent "trackingUrl")
  let eulerClient = Euler.client (Proxy @(Get '[JSON] GetLocationRes))
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  callApiUnwrappingApiError (identity @TrackUrlError) Nothing (Just "TRACK_URL_NOT_AVAILABLE") (Just internalEndPointHashMap) trackingUrl eulerClient "BPP.driverTrackUrl" (Proxy @(Get '[JSON] GetLocationRes))

feedback ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl]
  ) =>
  BaseUrl ->
  RatingReqV2 ->
  m RatingRes
feedback providerUrl req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  callBecknAPIWithSignature req.context.bap_id "feedback" API.ratingAPIV2 providerUrl internalEndPointHashMap req

callStatus ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl]
  ) =>
  BaseUrl ->
  StatusReq ->
  m StatusRes
callStatus providerUrl req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  callBecknAPIWithSignature req.context.bap_id "status" API.statusAPI providerUrl internalEndPointHashMap req

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
  HM.Map BaseUrl BaseUrl ->
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
  HM.Map BaseUrl BaseUrl ->
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
