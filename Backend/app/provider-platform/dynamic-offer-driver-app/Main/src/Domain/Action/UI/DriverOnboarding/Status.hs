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
  )
where

import Control.Lens ((^?), _head)
import qualified Domain.Types.DigilockerVerification as DDV
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import qualified Domain.Types.VehicleCategory as DVC
import Environment
import Kernel.Prelude
import Kernel.Types.Error hiding (Unauthorized)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverOnboarding.Status as SStatus
import Storage.Cac.TransporterConfig
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as SMOC
import qualified Storage.Queries.CommonDriverOnboardingDocuments as QCommonDoc
import qualified Storage.Queries.DigilockerVerification as QDV
import qualified Storage.Queries.Image as IQuery
import qualified Storage.Queries.Person as QPerson
import Utils.Common.Cac.KeyNameConstants

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

statusHandler :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Bool -> Maybe Bool -> Maybe DVC.VehicleCategory -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Flow StatusRes
statusHandler (personId, _merchantId, merchantOpCityId) makeSelfieAadhaarPanMandatory prefillData onboardingVehicleCategory useHVSdkForDL onlyMandatoryDocs useDriverLanguage = do
  merchantOperatingCity <- SMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  transporterConfig <- findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast personId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  driverImages <- IQuery.findAllByPersonId transporterConfig personId
  now <- getCurrentTime
  let driverImagesInfo = IQuery.DriverImagesInfo {driverId = personId, merchantOperatingCity, driverImages, transporterConfig, now}
      language = if useDriverLanguage == Just True then fromMaybe merchantOperatingCity.language person.language else merchantOperatingCity.language
  (dlStatus, mDL, dlVerficationMessage) <- SStatus.getDLAndStatus driverImagesInfo language useHVSdkForDL
  (rcStatus, _, rcVerficationMessage) <- SStatus.getRCAndStatus driverImagesInfo language
  (aadhaarStatus, _) <- SStatus.getAadhaarStatus personId
  let shouldActivateRc = True
  SStatus.StatusRes' {..} <- SStatus.statusHandler' (Just person) driverImagesInfo makeSelfieAadhaarPanMandatory prefillData onboardingVehicleCategory mDL useHVSdkForDL shouldActivateRc onlyMandatoryDocs

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

-- Helper function to get DigiLocker session status
-- Returns the sessionStatus directly from the latest DigilockerVerification record
getDigilockerOverallStatus :: Id SP.Person -> Flow (Maybe DDV.SessionStatus)
getDigilockerOverallStatus driverId = do
  latestSessions <- QDV.findLatestByDriverId (Just 1) (Just 0) driverId
  return $ fmap (.sessionStatus) (latestSessions ^? _head)
