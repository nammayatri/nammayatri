{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverBankAccountExtra where

import Domain.Types.DriverBankAccount
import Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import Sequelize as Se
import qualified Storage.Beam.DriverBankAccount as Beam
import Storage.Queries.OrphanInstances.DriverBankAccount

-- Extra code goes here --
getDrivers :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Kernel.Types.Id.Id Domain.Types.Person.Person] -> m [Domain.Types.DriverBankAccount.DriverBankAccount])
getDrivers driverIds = do findAllWithKV [Se.And [Se.Is Beam.driverId $ Se.In (Kernel.Types.Id.getId <$> driverIds)]]
