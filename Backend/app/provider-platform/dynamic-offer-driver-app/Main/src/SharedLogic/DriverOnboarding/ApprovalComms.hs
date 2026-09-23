{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | One message per onboarding entity, sent when its overall `approved` turns true.
--   Kept out of OnboardingComms: that module imports Guard -> Status -> Flow, and Flow is the
--   caller here, so it would be an import cycle.
module SharedLogic.DriverOnboarding.ApprovalComms
  ( notifyPersonApproved,
    notifyVehicleApproved,
  )
where

import qualified Domain.Types.MerchantMessage as DMM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.VehicleRegistrationCertificate as DRC
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification.Interface.Types as Notification
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified SharedLogic.DriverOnboarding as SDO
import SharedLogic.DriverOnboarding.OnboardingFlags.Types (OnboardingFlow)
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified Storage.CachedQueries.Merchant.MerchantMessage as QMM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverRCAssociation as QRCAssoc
import qualified Storage.Queries.Image as QImage
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Notifications as TN
import qualified Tools.SMS as Sms

onboardingApprovedKey :: Text
onboardingApprovedKey = "ONBOARDING_APPROVED"

vehicleApprovedKey :: Text
vehicleApprovedKey = "VEHICLE_APPROVED"

data ApprovedEntityData = ApprovedEntityData
  { personId :: Text,
    vehicleNo :: Maybe Text
  }
  deriving (Generic, ToJSON)

-- | A driver or fleet owner, on their own account's approval.
notifyPersonApproved :: OnboardingFlow m r => Id DMOC.MerchantOperatingCity -> DP.Person -> m ()
notifyPersonApproved merchantOpCityId person =
  notifyOnce ("OnboardingApprovedNotified:" <> person.id.getId) $
    sendApproved merchantOpCityId person onboardingApprovedKey DMM.ONBOARDING_APPROVED Nothing

-- | Goes to the person the RC image is filed under, else the RC's active driver.
notifyVehicleApproved :: OnboardingFlow m r => DRC.VehicleRegistrationCertificate -> Text -> m ()
notifyVehicleApproved rc registrationNo =
  whenJust rc.merchantOperatingCityId $ \merchantOpCityId ->
    notifyOnce ("VehicleApprovedNotified:" <> rc.id.getId) $ do
      mbImage <- QImage.findById rc.documentImageId
      mbRecipientId <- case mbImage of
        Just image -> pure (Just image.personId)
        Nothing -> fmap (.driverId) <$> QRCAssoc.findActiveAssociationByRC rc.id True
      mbRecipient <- maybe (pure Nothing) QPerson.findById mbRecipientId
      whenJust mbRecipient $ \recipient ->
        sendApproved merchantOpCityId recipient vehicleApprovedKey DMM.VEHICLE_APPROVED (Just registrationNo)

-- | Concurrent recomputes can both see the flip, so only the first one sends. A failure here must
--   never fail the recompute that wrote the flags.
notifyOnce :: OnboardingFlow m r => Text -> m () -> m ()
notifyOnce key action =
  withTryCatch "onboardingApproved:notify" (Hedis.setNxExpire key approvalNotifiedTtl True >>= flip when action) >>= \case
    Left err -> logError $ "Approval notification failed for " <> key <> ": " <> show err
    Right () -> pure ()
  where
    approvalNotifiedTtl :: Hedis.ExpirationTime
    approvalNotifiedTtl = 3600

-- | Push from merchant_push_notification; SMS from merchant_message when the city has dashboard SMS enabled. A missing row skips that channel.
sendApproved :: OnboardingFlow m r => Id DMOC.MerchantOperatingCity -> DP.Person -> Text -> DMM.MessageKey -> Maybe Text -> m ()
sendApproved merchantOpCityId recipient pnKey smsKey mbVehicleNo = do
  let params = ("name", recipient.firstName) : maybe [] (\v -> [("vehicleNo", v)]) mbVehicleNo
      entityData = ApprovedEntityData {personId = recipient.id.getId, vehicleNo = mbVehicleNo}
  if SDO.isFleetRole recipient.role
    then TN.notifyFleetOwnerOnMerchantPN merchantOpCityId recipient pnKey Notification.DRIVER_NOTIFY params entityData
    else TN.notifyDriverOnMerchantPN merchantOpCityId recipient pnKey Notification.DRIVER_NOTIFY params entityData
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  when transporterConfig.enableDashboardSms $ do
    mbMerchantMessage <- QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOpCityId smsKey Nothing Nothing
    case mbMerchantMessage of
      Nothing -> logInfo $ "Skipping " <> show smsKey <> " SMS for " <> recipient.id.getId <> ": no merchant_message row"
      Just merchantMessage -> do
        merchantOpCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
        mobileNumber <- mapM decrypt recipient.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
        let countryCode = fromMaybe (P.getCountryMobileCode merchantOpCity.country) recipient.mobileCountryCode
        buildSmsReq <- MessageBuilder.buildSendSmsReq merchantMessage params
        Sms.sendSMS recipient.merchantId merchantOpCityId (buildSmsReq (countryCode <> mobileNumber)) >>= Sms.checkSmsResult
