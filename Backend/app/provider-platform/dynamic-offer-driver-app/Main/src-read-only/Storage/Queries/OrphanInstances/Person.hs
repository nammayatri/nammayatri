{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Person where

import qualified Data.Text
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (KvDbFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Version
import qualified Storage.Beam.Person as Beam
import qualified Storage.Queries.Transformers.Person

instance FromTType' Beam.Person Domain.Types.Person.Person where
  fromTType' (Beam.PersonT {..}) = do
    backendConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> backendConfigVersion)
    clientBundleVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientBundleVersion)
    clientConfigVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientConfigVersion)
    clientSdkVersion' <- mapM Kernel.Utils.Version.readVersion (Data.Text.strip <$> clientSdkVersion)
    merchantOperatingCityId' <- Storage.Queries.Transformers.Person.getMerchantOpCId merchantId merchantOperatingCityId
    pure $
      Just
        Domain.Types.Person.Person
          { alternateMobileNumber = EncryptedHashed <$> (Encrypted <$> alternateMobileNumberEncrypted) <*> alternateMobileNumberHash,
            backendAppVersion = backendAppVersion,
            backendConfigVersion = backendConfigVersion',
            clientBundleVersion = clientBundleVersion',
            clientConfigVersion = clientConfigVersion',
            clientDevice = Kernel.Utils.Version.mkClientDevice clientOsType clientOsVersion,
            clientSdkVersion = clientSdkVersion',
            createdAt = createdAt,
            description = description,
            deviceToken = deviceToken,
            driverTag = driverTag,
            email = email,
            faceImageId = Kernel.Types.Id.Id <$> faceImageId,
            firstName = firstName,
            gender = gender,
            hometown = hometown,
            id = Kernel.Types.Id.Id id,
            identifier = identifier,
            identifierType = identifierType,
            isNew = isNew,
            language = language,
            languagesSpoken = languagesSpoken,
            lastName = lastName,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = merchantOperatingCityId',
            middleName = middleName,
            mobileCountryCode = mobileCountryCode,
            mobileNumber = EncryptedHashed <$> (Encrypted <$> mobileNumberEncrypted) <*> mobileNumberHash,
            onboardedFromDashboard = onboardedFromDashboard,
            passwordHash = passwordHash,
            rating = rating,
            registrationLat = registrationLat,
            registrationLon = registrationLon,
            role = role,
            totalEarnedCoins = totalEarnedCoins,
            unencryptedAlternateMobileNumber = unencryptedAlternateMobileNumber,
            unencryptedMobileNumber = unencryptedMobileNumber,
            updatedAt = updatedAt,
            useFakeOtp = useFakeOtp,
            usedCoins = usedCoins,
            whatsappNotificationEnrollStatus = whatsappNotificationEnrollStatus
          }

instance ToTType' Beam.Person Domain.Types.Person.Person where
  toTType' (Domain.Types.Person.Person {..}) = do
    Beam.PersonT
      { Beam.alternateMobileNumberEncrypted = alternateMobileNumber <&> unEncrypted . (.encrypted),
        Beam.alternateMobileNumberHash = alternateMobileNumber <&> (.hash),
        Beam.backendAppVersion = backendAppVersion,
        Beam.backendConfigVersion = fmap Kernel.Utils.Version.versionToText backendConfigVersion,
        Beam.clientBundleVersion = fmap Kernel.Utils.Version.versionToText clientBundleVersion,
        Beam.clientConfigVersion = fmap Kernel.Utils.Version.versionToText clientConfigVersion,
        Beam.clientOsType = clientDevice <&> (.deviceType),
        Beam.clientOsVersion = clientDevice <&> (.deviceVersion),
        Beam.clientSdkVersion = fmap Kernel.Utils.Version.versionToText clientSdkVersion,
        Beam.createdAt = createdAt,
        Beam.description = description,
        Beam.deviceToken = deviceToken,
        Beam.driverTag = driverTag,
        Beam.email = email,
        Beam.faceImageId = Kernel.Types.Id.getId <$> faceImageId,
        Beam.firstName = firstName,
        Beam.gender = gender,
        Beam.hometown = hometown,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.identifier = identifier,
        Beam.identifierType = identifierType,
        Beam.isNew = isNew,
        Beam.language = language,
        Beam.languagesSpoken = languagesSpoken,
        Beam.lastName = lastName,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Just $ Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.middleName = middleName,
        Beam.mobileCountryCode = mobileCountryCode,
        Beam.mobileNumberEncrypted = mobileNumber <&> unEncrypted . (.encrypted),
        Beam.mobileNumberHash = mobileNumber <&> (.hash),
        Beam.onboardedFromDashboard = onboardedFromDashboard,
        Beam.passwordHash = passwordHash,
        Beam.rating = rating,
        Beam.registrationLat = registrationLat,
        Beam.registrationLon = registrationLon,
        Beam.role = role,
        Beam.totalEarnedCoins = totalEarnedCoins,
        Beam.unencryptedAlternateMobileNumber = unencryptedAlternateMobileNumber,
        Beam.unencryptedMobileNumber = unencryptedMobileNumber,
        Beam.updatedAt = updatedAt,
        Beam.useFakeOtp = useFakeOtp,
        Beam.usedCoins = usedCoins,
        Beam.whatsappNotificationEnrollStatus = whatsappNotificationEnrollStatus
      }
