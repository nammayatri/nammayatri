module SharedLogic.Allocator.Jobs.AutoAccept.SendAutoAcceptOverlay where

import qualified Data.Aeson as A
import qualified Domain.Types.Booking as DRB
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.External.Types (Language (..))
import Kernel.Prelude (showBaseUrl)
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as HedisFlow
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import qualified SharedLogic.Allocator as SAllocator
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingExtra as QRBExtra
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Notifications as TN

autoAcceptRideOverlayKey :: Text
autoAcceptRideOverlayKey = "AUTO_ACCEPT_RIDE_CTA"

sendAutoAcceptOverlayToDriver ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    Esq.EsqDBReplicaFlow m r,
    HedisFlow.HedisLTSFlowEnv r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl]
  ) =>
  Job 'SAllocator.SendAutoAcceptOverlay ->
  m ExecutionResult
sendAutoAcceptOverlayToDriver (Job {id, jobInfo}) = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
  booking <- QRB.findById jobData.bookingId >>= fromMaybeM (BookingNotFound jobData.bookingId.getId)
  if booking.status == DRB.CANCELLED
    then do
      logInfo $ "Auto-accept overlay skipped: booking cancelled bookingId=" <> jobData.bookingId.getId
      pure Complete
    else do
      driver <- QPerson.findById jobData.driverId >>= fromMaybeM (PersonNotFound jobData.driverId.getId)
      mOverlay <-
        CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory
          jobData.merchantOperatingCityId
          autoAcceptRideOverlayKey
          (fromMaybe ENGLISH driver.language)
          Nothing
          Nothing
          Nothing
      selfBaseUrl <- asks (.selfBaseUrl)
      whenJust mOverlay $ \overlay -> do
        let overlayReq = buildOverlayReq selfBaseUrl jobData.bookingId (TN.mkOverlayReq overlay)
        TN.sendOverlay jobData.merchantOperatingCityId driver overlayReq
      now <- getCurrentTime
      QRBExtra.updateAutoAcceptOverlaySentAt jobData.bookingId now
      pure Complete

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
