{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Communication (module Storage.Queries.Communication, module ReExport) where

import qualified Domain.Types.Communication
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Communication as Beam
import Storage.Queries.CommunicationExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Communication.Communication -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Communication.Communication] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Communication.Communication -> m (Maybe Domain.Types.Communication.Communication))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findBySenderIdAndStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.Communication.CommunicationStatus -> m [Domain.Types.Communication.Communication])
findBySenderIdAndStatus senderId status = do findAllWithKV [Se.And [Se.Is Beam.senderId $ Se.Eq (Kernel.Types.Id.getId senderId), Se.Is Beam.status $ Se.Eq status]]

updateStatusById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Communication.CommunicationStatus -> Kernel.Types.Id.Id Domain.Types.Communication.Communication -> m ())
updateStatusById status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Communication.Communication -> m (Maybe Domain.Types.Communication.Communication))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
