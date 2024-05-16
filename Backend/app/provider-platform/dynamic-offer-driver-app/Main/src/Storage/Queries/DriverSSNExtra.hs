{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverSSNExtra where

import Domain.Types.DriverSSN
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverSSN as Beam
import Storage.Queries.OrphanInstances.DriverSSN

-- Extra code goes here --

upsert :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => (Domain.Types.DriverSSN.DriverSSN -> m ())
upsert driverSsn@(Domain.Types.DriverSSN.DriverSSN {..}) = do
  res <- findOneWithKV [Se.Is Beam.driverId $ Se.Eq (getId driverId)]
  if isJust res
    then
      updateWithKV
        [ Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
          Se.Set Beam.ssnEncrypted (ssn & unEncrypted . encrypted),
          Se.Set Beam.ssnHash (ssn & hash),
          Se.Set Beam.verificationStatus verificationStatus
        ]
        [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]
    else createWithKV driverSsn
