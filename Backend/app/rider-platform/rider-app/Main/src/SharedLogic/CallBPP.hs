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
import Beckn.Types.Core.Taxi.API.Track as API
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.Ride as DRide
import Environment
import qualified EulerHS.Types as Euler
import GHC.Records.Extra
import qualified Kernel.External.Maps.Types as MapSearch
import Kernel.Prelude
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError (IsBecknAPI)
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)
import Tools.Metrics (CoreMetrics)

search ::
  ( HasField "gatewayUrl" r BaseUrl,
    MonadFlow m,
    CoreMetrics m,
    HasBapInfo r m
  ) =>
  BaseUrl ->
  API.SearchReq ->
  m API.SearchRes
search gatewayUrl req = do
  callBecknAPIWithSignature "search" API.searchAPI gatewayUrl req

searchMetro ::
  ( HasField "gatewayUrl" r BaseUrl,
    MonadFlow m,
    CoreMetrics m,
    HasBapInfo r m
  ) =>
  BecknReq MigAPI.SearchIntent ->
  m ()
searchMetro req = do
  url <- asks (.gatewayUrl)
  void $ callBecknAPIWithSignatureMetro "search" MigAPI.searchAPI url req

select ::
  ( MonadFlow m,
    CoreMetrics m,
    HasBapInfo r m
  ) =>
  BaseUrl ->
  SelectReq ->
  m SelectRes
select = callBecknAPIWithSignature "select" API.selectAPI

init ::
  ( MonadFlow m,
    CoreMetrics m,
    HasBapInfo r m
  ) =>
  BaseUrl ->
  API.InitReq ->
  m API.InitRes
init = callBecknAPIWithSignature "init" API.initAPI

confirm ::
  ( MonadFlow m,
    CoreMetrics m,
    HasBapInfo r m
  ) =>
  BaseUrl ->
  ConfirmReq ->
  m ConfirmRes
confirm = callBecknAPIWithSignature "confirm" API.confirmAPI

cancel ::
  ( MonadFlow m,
    CoreMetrics m,
    HasBapInfo r m
  ) =>
  BaseUrl ->
  CancelReq ->
  m CancelRes
cancel = callBecknAPIWithSignature "cancel" API.cancelAPI

callTrack ::
  ( MonadFlow m,
    CoreMetrics m,
    HasBapInfo r m,
    HedisFlow m r
  ) =>
  DB.Booking ->
  DRide.Ride ->
  m ()
callTrack booking ride = do
  let trackBUildReq =
        TrackACL.TrackBuildReq
          { bppRideId = ride.bppRideId,
            bppId = booking.providerId,
            bppUrl = booking.providerUrl,
            transactionId = booking.transactionId
          }
  void . callBecknAPIWithSignature "track" API.trackAPI booking.providerUrl =<< TrackACL.buildTrackReq trackBUildReq

data Status = PreRide | ActualRide
  deriving (Generic, ToJSON, Show, FromJSON, ToSchema)

data GetLocationRes = GetLocationRes
  { currPoint :: MapSearch.LatLong,
    totalDistance :: Double,
    status :: Status,
    lastUpdate :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

callGetDriverLocation ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  DRide.Ride ->
  m GetLocationRes
callGetDriverLocation ride = do
  trackingUrl <- ride.trackingUrl & fromMaybeM (RideFieldNotPresent "trackingUrl")
  let eulerClient = Euler.client (Proxy @(Get '[JSON] GetLocationRes))
  callAPI trackingUrl eulerClient "BPP.driverTrackUrl"
    >>= fromEitherM (\err -> InternalError $ "Failed to call driverTrackUrl: " <> show err)

feedback ::
  ( MonadFlow m,
    CoreMetrics m,
    HasBapInfo r m
  ) =>
  BaseUrl ->
  RatingReq ->
  m RatingRes
feedback = callBecknAPIWithSignature "feedback" API.ratingAPI

callBecknAPIWithSignature,
  callBecknAPIWithSignatureMetro ::
    ( MonadFlow m,
      CoreMetrics m,
      IsBecknAPI api req res,
      HasBapInfo r m
    ) =>
    Text ->
    Proxy api ->
    BaseUrl ->
    req ->
    m res
callBecknAPIWithSignature a b c d = do
  bapId <- asks (.bapSelfIds.cabs)
  callBecknAPI (Just $ getHttpManagerKey bapId) Nothing a b c d
callBecknAPIWithSignatureMetro a b c d = do
  bapId <- asks (.bapSelfIds.metro)
  callBecknAPI (Just $ getHttpManagerKey bapId) Nothing a b c d
