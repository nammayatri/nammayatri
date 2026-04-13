{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SpecialZoneQueueRequestExtra where

import qualified Domain.Types.Person
import qualified Domain.Types.SpecialZoneQueueRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SpecialZoneQueueRequest as Beam
import Storage.Queries.OrphanInstances.SpecialZoneQueueRequest

-- Extra code goes here --
findActiveByStasusListAndDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  [Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequestStatus] ->
  m [Domain.Types.SpecialZoneQueueRequest.SpecialZoneQueueRequest]
findActiveByStasusListAndDriverId driverId statusList =
  findAllWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.status $ Se.In statusList]]
