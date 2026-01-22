{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Role (module Storage.Queries.Role, module ReExport) where

import qualified Data.Text
import qualified Domain.Types.Role
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Role as Beam
import Storage.Queries.RoleExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Role.Role -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Role.Role] -> m ())
createMany = traverse_ create

findAllByLimitOffset :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Role.Role -> m ([Domain.Types.Role.Role]))
findAllByLimitOffset limit offset id = do findAllWithOptionsKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)] (Se.Desc Beam.name) limit offset

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Role.Role -> m (Maybe Domain.Types.Role.Role))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByName :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> m (Maybe Domain.Types.Role.Role))
findByName name = do findOneWithKV [Se.Is Beam.name $ Se.Eq name]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Role.Role -> m (Maybe Domain.Types.Role.Role))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Role.Role -> m ())
updateByPrimaryKey (Domain.Types.Role.Role {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.description description,
      Se.Set Beam.name name,
      Se.Set Beam.needsBppAccountCreation needsBppAccountCreation,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
