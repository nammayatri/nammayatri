{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Person where

import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Person as Beam

instance FromTType' Beam.Person Domain.Types.Person.Person where
  fromTType' (Beam.PersonT {..}) = do
    pure $
      Just
        Domain.Types.Person.Person
          { approvedBy = Kernel.Types.Id.Id <$> approvedBy,
            createdAt = createdAt,
            email = EncryptedHashed <$> (Encrypted <$> emailEncrypted) <*> emailHash,
            firstName = firstName,
            id = Kernel.Types.Id.Id id,
            lastName = lastName,
            mobileCountryCode = mobileCountryCode,
            mobileNumber = EncryptedHashed <$> (Encrypted <$> mobileNumberEncrypted) <*> mobileNumberHash,
            passwordHash = passwordHash,
            passwordUpdatedAt = passwordUpdatedAt,
            receiveNotification = receiveNotification,
            rejectedAt = rejectedAt,
            rejectedBy = Kernel.Types.Id.Id <$> rejectedBy,
            rejectionReason = rejectionReason,
            roleId = Kernel.Types.Id.Id roleId,
            updatedAt = updatedAt,
            verified = verified
          }

instance ToTType' Beam.Person Domain.Types.Person.Person where
  toTType' (Domain.Types.Person.Person {..}) = do
    Beam.PersonT
      { Beam.approvedBy = Kernel.Types.Id.getId <$> approvedBy,
        Beam.createdAt = createdAt,
        Beam.emailEncrypted = ((email <&> unEncrypted . (.encrypted))),
        Beam.emailHash = (email <&> (.hash)),
        Beam.firstName = firstName,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lastName = lastName,
        Beam.mobileCountryCode = mobileCountryCode,
        Beam.mobileNumberEncrypted = ((mobileNumber <&> unEncrypted . (.encrypted))),
        Beam.mobileNumberHash = (mobileNumber <&> (.hash)),
        Beam.passwordHash = passwordHash,
        Beam.passwordUpdatedAt = passwordUpdatedAt,
        Beam.receiveNotification = receiveNotification,
        Beam.rejectedAt = rejectedAt,
        Beam.rejectedBy = Kernel.Types.Id.getId <$> rejectedBy,
        Beam.rejectionReason = rejectionReason,
        Beam.roleId = Kernel.Types.Id.getId roleId,
        Beam.updatedAt = updatedAt,
        Beam.verified = verified
      }
