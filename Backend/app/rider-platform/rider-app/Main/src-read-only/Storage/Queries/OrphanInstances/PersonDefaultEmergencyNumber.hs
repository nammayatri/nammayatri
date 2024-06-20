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
          { personId = Kernel.Types.Id.Id personId,
            name = name,
            mobileNumber = EncryptedHashed (Encrypted mobileNumberEncrypted) mobileNumberHash,
            mobileCountryCode = mobileCountryCode,
            createdAt = createdAt,
            contactPersonId = Kernel.Types.Id.Id <$> contactPersonId,
            enableForFollowing = enableForFollowing,
            enableForShareRide = enableForShareRide,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            priority = priority
          }

instance ToTType' Beam.PersonDefaultEmergencyNumber Domain.Types.PersonDefaultEmergencyNumber.PersonDefaultEmergencyNumber where
  toTType' (Domain.Types.PersonDefaultEmergencyNumber.PersonDefaultEmergencyNumber {..}) = do
    Beam.PersonDefaultEmergencyNumberT
      { Beam.personId = Kernel.Types.Id.getId personId,
        Beam.name = name,
        Beam.mobileNumberEncrypted = mobileNumber & unEncrypted . encrypted,
        Beam.mobileNumberHash = mobileNumber & hash,
        Beam.mobileCountryCode = mobileCountryCode,
        Beam.createdAt = createdAt,
        Beam.contactPersonId = Kernel.Types.Id.getId <$> contactPersonId,
        Beam.enableForFollowing = enableForFollowing,
        Beam.enableForShareRide = enableForShareRide,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.priority = priority
      }
