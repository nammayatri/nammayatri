{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DriverOnboarding.Status
  ( StatusRes (..),
    statusHandler,
    pullPendingDocStatuses,
  )
where

import qualified Data.Text as T
import qualified Domain.Action.UI.DriverOnboarding.DriverLicense as DDL
import qualified Domain.Action.UI.DriverOnboarding.GstVerification as DGst
import qualified Domain.Action.UI.DriverOnboarding.PanVerification as DPan
import qualified Domain.Action.UI.DriverOnboarding.SyncVerificationStatus as SyncV
import qualified Domain.Action.UI.DriverOnboarding.UdyamVerification as DUdyam
import qualified Domain.Types.DigilockerVerification as DDV
import qualified Domain.Types.DocumentVerificationConfig as DDVC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleCategory as DVC
import Environment
import qualified Kernel.External.Verification as KEV
import Kernel.Prelude
import Kernel.Types.Error hiding (Unauthorized)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified SharedLogic.DriverOnboarding as SDO
import qualified SharedLogic.DriverOnboarding.Status as SStatus
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as SMOC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.CommonDriverOnboardingDocuments as QCommonDoc
import qualified Storage.Queries.DigilockerVerification as QDV
import qualified Storage.Queries.Image as IQuery
import qualified Storage.Queries.Person as QPerson

data StatusRes = StatusRes
  { dlVerificationStatus :: SStatus.ResponseStatus, -- deprecated
    dlVerficationMessage :: Text, -- deprecated
    rcVerificationStatus :: SStatus.ResponseStatus, -- deprecated
    rcVerficationMessage :: Text, -- deprecated (not for MSIL)
    aadhaarVerificationStatus :: SStatus.ResponseStatus, -- deprecated

    --- use these fields
    driverDocuments :: [SStatus.DocumentStatusItem],
    vehicleDocuments :: [SStatus.VehicleDocumentItem],
    commonDocuments :: [SStatus.CommonDocumentItem],
    enabled :: Bool,
    manualVerificationRequired :: Maybe Bool,
    driverLicenseDetails :: Maybe [SStatus.DLDetails],
    vehicleRegistrationCertificateDetails :: Maybe [SStatus.RCDetails],
    digilockerStatus :: Maybe DDV.SessionStatus,
    digilockerResponseCode :: Maybe Text,
    digilockerAuthorizationUrl :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

statusHandler :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Bool -> Maybe Bool -> Maybe DVC.VehicleCategory -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Flow StatusRes
statusHandler (personId, _merchantId, merchantOpCityId) makeSelfieAadhaarPanMandatory prefillData onboardingVehicleCategory useHVSdkForDL onlyMandatoryDocs useDriverLanguage enableDocumentMetadata = do
  merchantOperatingCity <- SMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  now <- getCurrentTime
  let entity = IQuery.PersonEntity person
  entityImages <- IQuery.findAllByEntityId transporterConfig entity
  let entityImagesInfo = IQuery.EntityImagesInfo {entity, merchantOperatingCity, entityImages, transporterConfig, now, enableDocumentMetadata = fromMaybe False enableDocumentMetadata}
      language = if useDriverLanguage == Just True then fromMaybe merchantOperatingCity.language person.language else merchantOperatingCity.language
  (dlStatus, mDL, dlVerficationMessage) <- SStatus.getDLAndStatus personId entityImagesInfo language useHVSdkForDL
  (rcStatus, _, rcVerficationMessage) <- SStatus.getRCAndStatus personId entityImagesInfo language
  (aadhaarStatus, _) <- SStatus.getAadhaarStatus personId
  let shouldActivateRc = True
      skipMessages = False -- Need translations for API response
  SStatus.StatusRes' {..} <- SStatus.statusHandler' person entityImagesInfo makeSelfieAadhaarPanMandatory prefillData onboardingVehicleCategory mDL useHVSdkForDL shouldActivateRc onlyMandatoryDocs skipMessages

  -- Re-pull the latest provider status for any doc whose verification webhook was dropped, at the point
  -- its status is rendered. Shared with the dashboard status endpoint via 'pullPendingDocStatuses'.
  pullPendingDocStatuses transporterConfig person driverDocuments vehicleDocuments

  -- Fetch common documents
  commonDocumentsData <- QCommonDoc.findByDriverId (Just personId)
  let commonDocuments = map SStatus.mkCommonDocumentItem commonDocumentsData
  -- Check if DigiLocker is enabled for this merchant+city
  digilockerStatus <-
    if transporterConfig.digilockerEnabled == Just True
      then getDigilockerOverallStatus personId
      else pure Nothing

  pure $
    StatusRes
      { dlVerificationStatus = dlStatus,
        dlVerficationMessage = dlVerficationMessage,
        rcVerificationStatus = rcStatus,
        rcVerficationMessage = rcVerficationMessage,
        aadhaarVerificationStatus = aadhaarStatus,
        commonDocuments = commonDocuments,
        digilockerStatus = digilockerStatus,
        digilockerResponseCode = digilockerResponseCode,
        digilockerAuthorizationUrl = digilockerAuthorizationUrl,
        ..
      }

-- | Driver docs verified asynchronously (getTask + webhook), hence re-pullable. RC is handled separately.
asyncVerifiedDriverDocs :: [DDVC.DocumentType]
asyncVerifiedDriverDocs = [DDVC.DriverLicense, DDVC.PanCard, DDVC.GSTCertificate, DDVC.UDYAMCertificate]

-- | Fork a re-pull for every stuck doc in a rendered status (driver-app @statusHandler@ + dashboard
--   register-status). Gated per city on @enablePullPendingDocVerification@.
pullPendingDocStatuses :: DTC.TransporterConfig -> SP.Person -> [SStatus.DocumentStatusItem] -> [SStatus.VehicleDocumentItem] -> Flow ()
pullPendingDocStatuses transporterConfig person driverDocuments vehicleDocuments =
  when (transporterConfig.enablePullPendingDocVerification == Just True) $ do
    forM_ driverDocuments $ pullDriverDocStatusIfPending person
    forM_ vehicleDocuments $ \v -> forM_ v.documents $ pullVehicleDocStatusIfPending person

-- | Re-pull one driver doc's status (DL / PAN / GST / UDYAM), keyed by its image, if it's still pending.
pullDriverDocStatusIfPending :: SP.Person -> SStatus.DocumentStatusItem -> Flow ()
pullDriverDocStatusIfPending person d =
  when (d.documentType `elem` asyncVerifiedDriverDocs && SyncV.isPullableStatus d.verificationStatus) $
    fork "pullDriverDocStatus" $ pullDriverDocStatus person d.documentType d.imageId

-- | Re-pull one vehicle doc's (RC) status, keyed by the RC's image, if it's still pending.
pullVehicleDocStatusIfPending :: SP.Person -> SStatus.DocumentStatusItem -> Flow ()
pullVehicleDocStatusIfPending person d =
  when (d.documentType == DDVC.VehicleRegistrationCertificate && SyncV.isPullableStatus d.verificationStatus) $
    whenJust d.imageId $ \rcImageId -> fork "pullRcStatus" $ SyncV.pullRcStatus person rcImageId

-- | Re-pull + apply a stuck DL/PAN/GST/UDYAM verification. Lives here (not in @SyncVerificationStatus@)
--   because its @onVerify*@ dispatch would import-cycle back through that module.
pullDriverDocStatus :: SP.Person -> DDVC.DocumentType -> Maybe Text -> Flow ()
pullDriverDocStatus person dt mbImageId =
  SyncV.reconcilePending person dt mbImageId applyPulledStatus

-- | Apply a pulled @getTask@ result through the same @onVerify*@ handler the provider webhook uses.
--   A DL result that is still processing is a no-op — the next render re-pulls it.
applyPulledStatus :: SDO.VerificationReqRecord -> KEV.GetTaskResp -> KEV.VerificationService -> Flow ()
applyPulledStatus req resp service =
  case resp of
    KEV.DLResp o
      | "still being processed" `T.isInfixOf` fromMaybe "" o.message ->
        logInfo $ "pullDriverDocStatus: still processing " <> req.requestId
      | otherwise -> void $ DDL.onVerifyDL req o service
    KEV.PanResp o -> void $ DPan.onVerifyPan req o service
    KEV.GstResp o -> void $ DGst.onVerifyGst req o service
    KEV.UdyamAadhaarResp o -> void $ DUdyam.onVerifyUdyam req o service
    _ -> logInfo $ "pullDriverDocStatus: unhandled response type for reqId " <> req.requestId

-- Helper function to get DigiLocker session status
-- Returns the sessionStatus directly from the latest DigilockerVerification record
getDigilockerOverallStatus :: Id SP.Person -> Flow (Maybe DDV.SessionStatus)
getDigilockerOverallStatus driverId = do
  latestSessions <- QDV.findLatestByDriverId (Just 1) (Just 0) driverId
  return $ fmap (.sessionStatus) (listToMaybe latestSessions)
