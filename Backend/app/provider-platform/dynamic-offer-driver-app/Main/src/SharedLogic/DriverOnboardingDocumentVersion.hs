{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The details each DL/RC version was registered or approved with, keyed by image.documentVersionId.
module SharedLogic.DriverOnboardingDocumentVersion where

import qualified Domain.Types.DocumentVerificationConfig as ODC
import qualified Domain.Types.DocumentVersionMetadata as DDVM
import qualified Domain.Types.DriverLicense as DDL
import qualified Domain.Types.Image as Image
import Domain.Types.VehicleRegistrationCertificate (VehicleRegistrationCertificate)
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.DriverOnboarding.OnboardingFlags.Types (OnboardingFlow)
import SharedLogic.DriverOnboarding.VehicleDocs (DLDocumentMetadata (..), DocumentMetadata (..), RCDocumentMetadata (..))
import qualified Storage.Queries.DocumentVersionMetadata as QDVM
import qualified Storage.Queries.Image as ImageQuery

-- | A DL row's details belong to the version whose images it holds.
saveDLVersionMetadata :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DDL.DriverLicense -> m ()
saveDLVersionMetadata dl =
  saveVersionMetadata dl.documentImageId1 $ \frontImage versionId now ->
    DDVM.DocumentVersionMetadata
      { DDVM.documentVersionId = versionId,
        DDVM.documentType = ODC.DriverLicense,
        DDVM.personId = dl.driverId,
        DDVM.documentNumber = dl.licenseNumber,
        DDVM.metadata =
          Just $
            DDVM.DLMetadata
              DDVM.DLVersionMetadata
                { DDVM.driverDateOfBirth = dl.driverDob,
                  DDVM.dateOfExpiry = Just dl.licenseExpiry
                },
        DDVM.merchantId = Just frontImage.merchantId,
        DDVM.merchantOperatingCityId = frontImage.merchantOperatingCityId,
        DDVM.createdAt = now,
        DDVM.updatedAt = now
      }

-- | An RC row's details belong to the version whose images it holds.
saveRCVersionMetadata :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => VehicleRegistrationCertificate -> m ()
saveRCVersionMetadata rc =
  saveVersionMetadata rc.documentImageId $ \frontImage versionId now ->
    DDVM.DocumentVersionMetadata
      { DDVM.documentVersionId = versionId,
        DDVM.documentType = ODC.VehicleRegistrationCertificate,
        DDVM.personId = frontImage.personId,
        DDVM.documentNumber = rc.certificateNumber,
        DDVM.metadata =
          Just $
            DDVM.RCMetadata
              DDVM.RCVersionMetadata
                { DDVM.fitnessExpiry = Just rc.fitnessExpiry,
                  DDVM.vehicleVariant = rc.vehicleVariant,
                  DDVM.vehicleManufacturer = rc.vehicleManufacturer,
                  DDVM.vehicleModel = rc.vehicleModel,
                  DDVM.vehicleModelYear = rc.vehicleModelYear,
                  DDVM.vehicleColor = rc.vehicleColor
                },
        DDVM.merchantId = Just frontImage.merchantId,
        DDVM.merchantOperatingCityId = frontImage.merchantOperatingCityId,
        DDVM.createdAt = now,
        DDVM.updatedAt = now
      }

-- Images registered before versioning belong to no version, so they get no copy.
saveVersionMetadata :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id Image.Image -> (Image.Image -> Text -> UTCTime -> DDVM.DocumentVersionMetadata) -> m ()
saveVersionMetadata frontImageId buildMetadata = do
  mbFrontImage <- ImageQuery.findById frontImageId
  whenJust mbFrontImage $ \frontImage ->
    whenJust frontImage.documentVersionId $ \versionId -> do
      now <- getCurrentTime
      let versionMetadata = buildMetadata frontImage versionId now
      QDVM.findByPrimaryKey versionId >>= \case
        Just existing -> QDVM.updateByPrimaryKey versionMetadata {DDVM.createdAt = existing.createdAt}
        Nothing -> QDVM.create versionMetadata

-- | The details the version holding this image was registered or approved with.
findVersionMetadata :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id Image.Image -> m (Maybe DDVM.DocumentVersionMetadata)
findVersionMetadata imageId = do
  mbVersionId <- (>>= (.documentVersionId)) <$> ImageQuery.findById imageId
  maybe (pure Nothing) QDVM.findByPrimaryKey mbVersionId

mkVersionMetadata :: OnboardingFlow m r => Maybe Text -> DDVM.DocumentVersionMetadata -> m (Maybe DocumentMetadata)
mkVersionMetadata mbImageId versionMetadata =
  forM versionMetadata.metadata $ \storedMetadata -> do
    documentNumber <- decrypt versionMetadata.documentNumber
    pure $ case storedMetadata of
      DDVM.DLMetadata dlMetadata ->
        DLMetadata DLDocumentMetadata {driverLicenseNumber = documentNumber, driverDateOfBirth = dlMetadata.driverDateOfBirth, dateOfExpiry = dlMetadata.dateOfExpiry, imageId1 = mbImageId, imageId2 = Nothing}
      DDVM.RCMetadata rcMetadata ->
        RCMetadata
          RCDocumentMetadata
            { fitnessExpiry = rcMetadata.fitnessExpiry,
              vehicleNumberPlate = documentNumber,
              vehicleVariant = show <$> rcMetadata.vehicleVariant,
              vehicleManufacturer = rcMetadata.vehicleManufacturer,
              vehicleModel = rcMetadata.vehicleModel,
              vehicleModelYear = rcMetadata.vehicleModelYear,
              vehicleColor = rcMetadata.vehicleColor,
              imageId = mbImageId,
              imageId2 = Nothing
            }
