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
import qualified Storage.Queries.Image as IQuery
import Utils.Common.Cac.KeyNameConstants

data StatusRes = StatusRes
  { dlVerificationStatus :: SStatus.ResponseStatus, -- deprecated
    dlVerficationMessage :: Text, -- deprecated
    rcVerificationStatus :: SStatus.ResponseStatus, -- deprecated
    rcVerficationMessage :: Text, -- deprecated
    aadhaarVerificationStatus :: SStatus.ResponseStatus, -- deprecated

    --- use these fields
    driverDocuments :: [SStatus.DocumentStatusItem],
    vehicleDocuments :: [SStatus.VehicleDocumentItem],
    enabled :: Bool,
    manualVerificationRequired :: Maybe Bool,
    driverLicenseDetails :: Maybe [SStatus.DLDetails],
    vehicleRegistrationCertificateDetails :: Maybe [SStatus.RCDetails]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

statusHandler :: (Id SP.Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe DVC.VehicleCategory -> Maybe Bool -> Flow StatusRes
statusHandler (personId, _merchantId, merchantOpCityId) makeSelfieAadhaarPanMandatory multipleRC prefillData onboardingVehicleCategory useHVSdkForDL = do
  -- multipleRC flag is temporary to support backward compatibility
  merchantOperatingCity <- SMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
  transporterConfig <- findByMerchantOpCityId merchantOpCityId (Just (DriverId (cast personId))) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  driverImages <- IQuery.findAllByPersonId transporterConfig personId
  now <- getCurrentTime
  let driverImagesInfo = IQuery.DriverImagesInfo {driverId = personId, merchantOperatingCity, driverImages, transporterConfig, now}
  (dlStatus, mDL, dlVerficationMessage) <- SStatus.getDLAndStatus driverImagesInfo merchantOperatingCity.language useHVSdkForDL
  (rcStatus, _, rcVerficationMessage) <- SStatus.getRCAndStatus driverImagesInfo multipleRC merchantOperatingCity.language
  (aadhaarStatus, _) <- SStatus.getAadhaarStatus personId
  let shouldActivateRc = True
  SStatus.StatusRes' {..} <- SStatus.statusHandler' driverImagesInfo makeSelfieAadhaarPanMandatory multipleRC prefillData onboardingVehicleCategory mDL useHVSdkForDL shouldActivateRc
  pure $
    StatusRes
      { dlVerificationStatus = dlStatus,
        dlVerficationMessage = dlVerficationMessage,
        rcVerificationStatus = rcStatus,
        rcVerficationMessage = rcVerficationMessage,
        aadhaarVerificationStatus = aadhaarStatus,
        ..
      }
