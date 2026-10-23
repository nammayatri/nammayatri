{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | "Your vehicle is now linked to another account" notice for a driver who lost an RC to another driver.
--   The overlay copy lives in merchant_overlay (one key per case, per city and language). The same
--   overlay is pushed live (DRIVER_NOTIFY, driver_notification_payload) and returned on the driver
--   profile until the driver acknowledges it or activates a vehicle.
module SharedLogic.RCTransferNotice
  ( RCTransferNoticeRes (..),
    RCTransferOverlay (..),
    RCTransferNoticeAckReq (..),
    recordAndNotifyRCTransfer,
    getPendingRCTransferNotice,
    acknowledgeRCTransferNotice,
    clearPendingRCTransferNotices,
  )
where

import Data.Aeson (object, (.=))
import Data.List (sortOn)
import Data.Ord (Down (..))
import qualified Data.Text as T
import qualified Domain.Types.Person as DP
import qualified Domain.Types.RCTransferNotice as DRTN
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import Kernel.External.Encryption
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.External.Types (Language (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Documents (VerificationStatus (VALID))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.JSON (removeNullFields)
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import qualified Storage.Queries.DriverRCAssociation as DAQuery
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RCTransferNotice as QRTN
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import qualified Tools.Notifications as Notify

data RCTransferNoticeRes = RCTransferNoticeRes
  { transferId :: Id DRTN.RCTransferNotice,
    overlay :: RCTransferOverlay
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

-- | Typed mirror of FCM.FCMOverlayNotificationJSON (which has no ToSchema), so the profile API spec
--   describes the overlay. Serialises to the same JSON as the push's driver_notification_payload.
data RCTransferOverlay = RCTransferOverlay
  { title :: Maybe Text,
    description :: Maybe Text,
    imageUrl :: Maybe Text,
    okButtonText :: Maybe Text,
    cancelButtonText :: Maybe Text,
    actions :: [Text],
    link :: Maybe Text,
    method :: Maybe Text,
    reqBody :: Value,
    actions2 :: [FCM.FCMActions],
    secondaryActions2 :: Maybe [FCM.FCMActions],
    endPoint :: Maybe Text,
    titleVisibility :: Bool,
    descriptionVisibility :: Bool,
    buttonOkVisibility :: Bool,
    buttonCancelVisibility :: Bool,
    buttonLayoutVisibility :: Bool,
    imageVisibility :: Bool,
    delay :: Maybe Int,
    contactSupportNumber :: Maybe Text,
    toastMessage :: Maybe Text,
    secondaryActions :: Maybe [Text],
    socialMediaLinks :: Maybe [FCM.FCMMediaLink],
    showPushNotification :: Maybe Bool
  }
  deriving (Generic, ToSchema)

instance ToJSON RCTransferOverlay where
  toJSON = genericToJSON removeNullFields

instance FromJSON RCTransferOverlay where
  parseJSON = genericParseJSON removeNullFields

-- | Same derivation as FCM.createAndroidOverlayNotification.
mkRCTransferOverlay :: FCM.FCMOverlayReq -> RCTransferOverlay
mkRCTransferOverlay FCM.FCMOverlayReq {..} =
  RCTransferOverlay
    { titleVisibility = isJust title,
      descriptionVisibility = isJust description,
      buttonOkVisibility = isJust okButtonText,
      buttonCancelVisibility = isJust cancelButtonText,
      buttonLayoutVisibility = isJust okButtonText || isJust cancelButtonText,
      imageVisibility = isJust imageUrl,
      ..
    }

newtype RCTransferNoticeAckReq = RCTransferNoticeAckReq
  { transferId :: Id DRTN.RCTransferNotice
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

-- | Persist the transfer, then push the overlay to the previous holder.
--   Call only after the new driver's activation has succeeded.
recordAndNotifyRCTransfer ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r, Redis.HedisLTSFlowEnv r, HasFlowEnv m r '["selfUIUrl" ::: BaseUrl]) =>
  Id DP.Person ->
  Id DP.Person ->
  DVRC.VehicleRegistrationCertificate ->
  m ()
recordAndNotifyRCTransfer previousDriverId newDriverId rc = do
  previousDriver <- QPerson.findById previousDriverId >>= fromMaybeM (PersonNotFound previousDriverId.getId)
  otherRcIds <- filter (/= rc.id) . map (.rcId) <$> DAQuery.findAllLinkedByDriverId previousDriverId
  hasOtherVehicles <- any ((== VALID) . (.verificationStatus)) <$> RCQuery.findAllById otherRcIds
  id <- generateGUID
  now <- getCurrentTime
  let notice =
        DRTN.RCTransferNotice
          { id,
            rcId = rc.id,
            previousDriverId,
            newDriverId,
            hasOtherVehicles,
            acknowledged = False,
            merchantId = previousDriver.merchantId,
            merchantOperatingCityId = previousDriver.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }
  QRTN.create notice
  logTagInfo "RC_TRANSFER" $
    "transferId: " <> id.getId <> ", rcId: " <> rc.id.getId <> ", fromDriverId: " <> previousDriverId.getId
      <> ", toDriverId: "
      <> newDriverId.getId
      <> ", merchantOperatingCityId: "
      <> previousDriver.merchantOperatingCityId.getId
      <> ", hasOtherVehicles: "
      <> show hasOtherVehicles
  mbOverlayReq <- buildOverlayReq previousDriver.language notice rc
  case mbOverlayReq of
    Nothing -> pure ()
    Just overlayReq -> Notify.sendOverlay previousDriver.merchantOperatingCityId previousDriver overlayReq

-- | Latest unacknowledged notice for the driver, rendered in their language. Nothing when none is pending,
--   the city has no overlay configured, or building it fails: the notice is optional and must never fail
--   the driver profile it is attached to.
getPendingRCTransferNotice ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r, HasFlowEnv m r '["selfUIUrl" ::: BaseUrl]) =>
  Id DP.Person ->
  Maybe Language ->
  m (Maybe RCTransferNoticeRes)
getPendingRCTransferNotice driverId mbLanguage = do
  result <- withTryCatch "getPendingRCTransferNotice" $ do
    pending <- QRTN.findAllByPreviousDriverIdAndAcknowledged driverId False
    case sortOn (Down . (.createdAt)) pending of
      [] -> pure Nothing
      notice : _ -> do
        mbRc <- RCQuery.findById notice.rcId
        mbOverlayReq <- maybe (pure Nothing) (buildOverlayReq mbLanguage notice) mbRc
        pure $ (\overlayReq -> RCTransferNoticeRes {transferId = notice.id, overlay = mkRCTransferOverlay overlayReq}) <$> mbOverlayReq
  case result of
    Right mbNotice -> pure mbNotice
    Left err -> do
      logError $ "Failed to build pending RC transfer notice for driver " <> driverId.getId <> ": " <> show err
      pure Nothing

-- | Dismisses every pending notice of the driver; the latest one supersedes older ones.
acknowledgeRCTransferNotice :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> RCTransferNoticeAckReq -> m APISuccess
acknowledgeRCTransferNotice driverId req = do
  notice <- QRTN.findById req.transferId >>= fromMaybeM (InvalidRequest "RC transfer notice not found")
  unless (notice.previousDriverId == driverId) $ throwError (InvalidRequest "RC transfer notice not found")
  clearPendingRCTransferNotices driverId
  pure Success

-- | The driver has a vehicle again, so any "vehicle taken away" notice is moot.
clearPendingRCTransferNotices :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> m ()
clearPendingRCTransferNotices driverId = do
  pending <- QRTN.findAllByPreviousDriverIdAndAcknowledged driverId False
  unless (null pending) $ QRTN.updateAcknowledgedByPreviousDriverId True driverId

buildOverlayReq ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r, HasFlowEnv m r '["selfUIUrl" ::: BaseUrl]) =>
  Maybe Language ->
  DRTN.RCTransferNotice ->
  DVRC.VehicleRegistrationCertificate ->
  m (Maybe FCM.FCMOverlayReq)
buildOverlayReq mbLanguage notice rc = do
  let overlayKey = if notice.hasOtherVehicles then "RC_TRANSFERRED_VEHICLE_REMOVED" else "RC_TRANSFERRED_NO_VEHICLE"
  mbOverlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory notice.merchantOperatingCityId overlayKey (fromMaybe ENGLISH mbLanguage) Nothing Nothing Nothing
  case mbOverlay of
    Nothing -> do
      logWarning $ "merchant_overlay " <> overlayKey <> " not configured for city " <> notice.merchantOperatingCityId.getId <> ", skipping RC transfer overlay for driver " <> notice.previousDriverId.getId
      pure Nothing
    Just overlay -> do
      maskedRcNo <- maskText <$> decrypt rc.certificateNumber
      selfUIUrl <- asks (.selfUIUrl)
      let fillRcNo = T.replace "{#rcNumber#}" maskedRcNo
          overlayReq = Notify.mkOverlayReq overlay
      pure . Just $
        overlayReq
          { FCM.title = fillRcNo <$> overlayReq.title,
            FCM.description = fillRcNo <$> overlayReq.description,
            FCM.toastMessage = fillRcNo <$> overlayReq.toastMessage,
            -- The app's CALL_API needs an absolute URL, so the ack endpoint is built here rather than configured.
            FCM.endPoint = Just . showBaseUrl $ selfUIUrl {baseUrlPath = baseUrlPath selfUIUrl <> "/driver/rcTransferNotice/ack"},
            FCM.method = Just "POST",
            FCM.reqBody = object ["transferId" .= notice.id]
          }
