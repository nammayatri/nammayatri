{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Permit.Storage.Queries.OrphanInstances.PermitApplicant where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Permit.Domain.Types.PermitApplicant
import qualified Lib.Permit.Storage.Beam.PermitApplicant as Beam

instance FromTType' Beam.PermitApplicant Lib.Permit.Domain.Types.PermitApplicant.PermitApplicant where
  fromTType' (Beam.PermitApplicantT {..}) = do
    pure $
      Just
        Lib.Permit.Domain.Types.PermitApplicant.PermitApplicant
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            mobileCountryCode = mobileCountryCode,
            mobileNumber = EncryptedHashed (Encrypted mobileNumberEncrypted) mobileNumberHash,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PermitApplicant Lib.Permit.Domain.Types.PermitApplicant.PermitApplicant where
  toTType' (Lib.Permit.Domain.Types.PermitApplicant.PermitApplicant {..}) = do
    Beam.PermitApplicantT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.mobileCountryCode = mobileCountryCode,
        Beam.mobileNumberEncrypted = ((mobileNumber & unEncrypted . encrypted)),
        Beam.mobileNumberHash = (mobileNumber & hash),
        Beam.updatedAt = updatedAt
      }
