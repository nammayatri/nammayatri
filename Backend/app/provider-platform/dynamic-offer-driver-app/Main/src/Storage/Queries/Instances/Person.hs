{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Instances.Person where

import qualified Database.Beam.Query ()
import Domain.Types.Person as Person
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common hiding (Value)
import Kernel.Utils.Version
import qualified Storage.Beam.Person as BeamP
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.Queries.Booking ()
import qualified Storage.Queries.DriverOnboarding.DriverLicense ()
import qualified Storage.Queries.DriverOnboarding.DriverRCAssociation ()
import qualified Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate ()
import Storage.Queries.DriverQuote ()
import Storage.Queries.Instances.DriverInformation ()
import Storage.Queries.Ride ()
import Storage.Queries.Vehicle ()
import Tools.Error

instance FromTType' BeamP.Person Person where
  fromTType' :: (L.MonadFlow m, Log m, CacheFlow m r, EsqDBFlow m r) => BeamP.Person -> m (Maybe Person)
  fromTType' BeamP.PersonT {..} = do
    bundleVersion' <- forM bundleVersion readVersion
    clientVersion' <- forM clientVersion readVersion
    merchant <- CQM.findById (Id merchantId) >>= fromMaybeM (MerchantNotFound merchantId)
    merchantOpCityId <- CQMOC.getMerchantOpCityId (Id <$> merchantOperatingCityId) merchant Nothing
    pure $
      Just
        Person
          { id = Id id,
            firstName = firstName,
            middleName = middleName,
            lastName = lastName,
            role = role,
            gender = gender,
            hometown = hometown,
            languagesSpoken = languagesSpoken,
            identifierType = identifierType,
            email = email,
            unencryptedMobileNumber = unencryptedMobileNumber,
            mobileNumber = EncryptedHashed <$> (Encrypted <$> mobileNumberEncrypted) <*> mobileNumberHash,
            onboardedFromDashboard = onboardedFromDashboard,
            mobileCountryCode = mobileCountryCode,
            passwordHash = passwordHash,
            identifier = identifier,
            rating = rating,
            isNew = isNew,
            merchantId = Id merchantId,
            merchantOperatingCityId = merchantOpCityId,
            deviceToken = deviceToken,
            whatsappNotificationEnrollStatus = whatsappNotificationEnrollStatus,
            language = language,
            description = description,
            createdAt = createdAt,
            updatedAt = updatedAt,
            bundleVersion = bundleVersion',
            clientVersion = clientVersion',
            unencryptedAlternateMobileNumber = unencryptedAlternateMobileNumber,
            faceImageId = Id <$> faceImageId,
            alternateMobileNumber = EncryptedHashed <$> (Encrypted <$> alternateMobileNumberEncrypted) <*> alternateMobileNumberHash,
            totalEarnedCoins = totalEarnedCoins,
            usedCoins = usedCoins,
            registrationLat = registrationLat,
            registrationLon = registrationLon
          }

instance ToTType' BeamP.Person Person where
  toTType' Person {..} = do
    BeamP.PersonT
      { BeamP.id = getId id,
        BeamP.firstName = firstName,
        BeamP.middleName = middleName,
        BeamP.lastName = lastName,
        BeamP.role = role,
        BeamP.gender = gender,
        BeamP.hometown = hometown,
        BeamP.languagesSpoken = languagesSpoken,
        BeamP.identifierType = identifierType,
        BeamP.email = email,
        BeamP.unencryptedMobileNumber = unencryptedMobileNumber,
        BeamP.mobileNumberEncrypted = mobileNumber <&> unEncrypted . (.encrypted),
        BeamP.onboardedFromDashboard = onboardedFromDashboard,
        BeamP.mobileNumberHash = mobileNumber <&> (.hash),
        BeamP.mobileCountryCode = mobileCountryCode,
        BeamP.passwordHash = passwordHash,
        BeamP.identifier = identifier,
        BeamP.rating = rating,
        BeamP.isNew = isNew,
        BeamP.merchantId = getId merchantId,
        BeamP.merchantOperatingCityId = Just $ getId merchantOperatingCityId,
        BeamP.deviceToken = deviceToken,
        BeamP.whatsappNotificationEnrollStatus = whatsappNotificationEnrollStatus,
        BeamP.language = language,
        BeamP.description = description,
        BeamP.createdAt = createdAt,
        BeamP.updatedAt = updatedAt,
        BeamP.bundleVersion = versionToText <$> bundleVersion,
        BeamP.clientVersion = versionToText <$> clientVersion,
        BeamP.unencryptedAlternateMobileNumber = unencryptedAlternateMobileNumber,
        BeamP.alternateMobileNumberHash = alternateMobileNumber <&> (.hash),
        BeamP.faceImageId = getId <$> faceImageId,
        BeamP.alternateMobileNumberEncrypted = alternateMobileNumber <&> unEncrypted . (.encrypted),
        BeamP.totalEarnedCoins = totalEarnedCoins,
        BeamP.usedCoins = usedCoins,
        BeamP.registrationLat = registrationLat,
        BeamP.registrationLon = registrationLon
      }
