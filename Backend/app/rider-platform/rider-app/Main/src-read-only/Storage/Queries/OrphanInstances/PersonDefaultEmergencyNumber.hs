{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.PersonDefaultEmergencyNumber where

import qualified Domain.Types.PersonDefaultEmergencyNumber
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.PersonDefaultEmergencyNumber as Beam

instance FromTType' Beam.PersonDefaultEmergencyNumber Domain.Types.PersonDefaultEmergencyNumber.PersonDefaultEmergencyNumber where
  fromTType' (Beam.PersonDefaultEmergencyNumberT {..}) = do
    pure $
      Just
        Domain.Types.PersonDefaultEmergencyNumber.PersonDefaultEmergencyNumber
          { contactPersonId = Kernel.Types.Id.Id <$> contactPersonId,
            createdAt = createdAt,
            enableForFollowing = enableForFollowing,
            enableForShareRide = enableForShareRide,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            mobileCountryCode = mobileCountryCode,
            mobileNumber = EncryptedHashed (Encrypted mobileNumberEncrypted) mobileNumberHash,
            name = name,
            personId = Kernel.Types.Id.Id personId,
            priority = priority,
            shareTripWithEmergencyContactOption = shareTripWithEmergencyContactOption
          }

instance ToTType' Beam.PersonDefaultEmergencyNumber Domain.Types.PersonDefaultEmergencyNumber.PersonDefaultEmergencyNumber where
  toTType' (Domain.Types.PersonDefaultEmergencyNumber.PersonDefaultEmergencyNumber {..}) = do
    Beam.PersonDefaultEmergencyNumberT
      { Beam.contactPersonId = Kernel.Types.Id.getId <$> contactPersonId,
        Beam.createdAt = createdAt,
        Beam.enableForFollowing = enableForFollowing,
        Beam.enableForShareRide = enableForShareRide,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.mobileCountryCode = mobileCountryCode,
        Beam.mobileNumberEncrypted = mobileNumber & unEncrypted . encrypted,
        Beam.mobileNumberHash = mobileNumber & hash,
        Beam.name = name,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.priority = priority,
        Beam.shareTripWithEmergencyContactOption = shareTripWithEmergencyContactOption
      }
