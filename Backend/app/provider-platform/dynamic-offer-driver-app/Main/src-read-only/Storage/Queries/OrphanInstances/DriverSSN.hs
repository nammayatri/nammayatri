{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DriverSSN where

import qualified Domain.Types.DriverSSN
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DriverSSN as Beam

instance FromTType' Beam.DriverSSN Domain.Types.DriverSSN.DriverSSN where
  fromTType' (Beam.DriverSSNT {..}) = do
    pure $
      Just
        Domain.Types.DriverSSN.DriverSSN
          { driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            rejectReason = rejectReason,
            ssn = EncryptedHashed (Encrypted ssnEncrypted) ssnHash,
            verificationStatus = verificationStatus
          }

instance ToTType' Beam.DriverSSN Domain.Types.DriverSSN.DriverSSN where
  toTType' (Domain.Types.DriverSSN.DriverSSN {..}) = do
    Beam.DriverSSNT
      { Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.rejectReason = rejectReason,
        Beam.ssnEncrypted = ((ssn & unEncrypted . encrypted)),
        Beam.ssnHash = (ssn & hash),
        Beam.verificationStatus = verificationStatus
      }
