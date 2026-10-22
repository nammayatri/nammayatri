module SharedLogic.Allocator.Jobs.AutoAccept.SendAutoAcceptOverlay where

import qualified Data.Aeson as A
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Person as DP
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.External.Types (Language (..), ServiceFlow)
import Kernel.Prelude (showBaseUrl)
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as HedisFlow
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingExtra as QRBExtra
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Notifications as TN

autoAcceptRideOverlayKey :: Text
autoAcceptRideOverlayKey = "AUTO_ACCEPT_RIDE_CTA"

sendAutoAcceptOverlay ::
  ( ServiceFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    Esq.EsqDBReplicaFlow m r,
    HedisFlow.HedisFlow m r,
    HedisFlow.HedisLTSFlowEnv r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    HasFlowEnv m r '["maxNotificationShards" ::: Int]
  ) =>
  Id DRB.Booking ->
  Id DP.Person ->
  m ()
sendAutoAcceptOverlay bookingId driverId = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingNotFound bookingId.getId)
  when (booking.status /= DRB.CANCELLED) $ do
    driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
    mOverlay <-
      CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory
        booking.merchantOperatingCityId
        autoAcceptRideOverlayKey
        (fromMaybe ENGLISH driver.language)
        Nothing
        Nothing
        Nothing
    selfBaseUrl <- asks (.selfBaseUrl)
    whenJust mOverlay $ \overlay -> do
      let overlayReq = buildOverlayReq selfBaseUrl bookingId (TN.mkOverlayReq overlay)
      TN.sendAutoAcceptRideCTAOverlay driver overlayReq
    now <- getCurrentTime
    let newCtaData = Just DRB.AutoAcceptCtaData {overlaySentAt = Just now, acknowledged = booking.autoAcceptCtaData >>= (.acknowledged)}
    QRBExtra.updateAutoAcceptCtaData bookingId newCtaData

buildOverlayReq :: BaseUrl -> Id DRB.Booking -> FCM.FCMOverlayReq -> FCM.FCMOverlayReq
buildOverlayReq selfBaseUrl bookingId baseReq =
  let endpointUrl = showBaseUrl selfBaseUrl <> "/ui/driver/autoAcceptRide/" <> getId bookingId <> "/acknowledge"
      callApiAction =
        FCM.FCMActions
          { primaryAction =
              FCM.CALL_API
                FCM.CallAPIDetails
                  { endPoint = endpointUrl,
                    method = "POST",
                    reqBody = A.object ["acknowledged" A..= True]
                  },
            dependentActions = []
          }
   in baseReq {FCM.actions2 = [callApiAction]}
