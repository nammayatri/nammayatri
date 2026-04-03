{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.AccessMatrix where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.AccessMatrix
import qualified Storage.Beam.AccessMatrix as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Domain.Types.Role
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AccessMatrix.AccessMatrix -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.AccessMatrix.AccessMatrix] -> m ())
createMany = traverse_ create
findAllByRoleId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Role.Role -> m ([Domain.Types.AccessMatrix.AccessMatrix]))
findAllByRoleId roleId = do findAllWithKV [Se.Is Beam.roleId $ Se.Eq (Kernel.Types.Id.getId roleId)]
findAllByRoles :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Role.Role -> m ([Domain.Types.AccessMatrix.AccessMatrix]))
findAllByRoles roleId = do findAllWithKV [Se.Is Beam.roleId $ Se.Eq (Kernel.Types.Id.getId roleId)]
findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.AccessMatrix.AccessMatrix -> m (Maybe Domain.Types.AccessMatrix.AccessMatrix))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
findByRoleIdAndServerAndActionType :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                                      (Kernel.Types.Id.Id Domain.Types.Role.Role -> Kernel.Prelude.Maybe Domain.Types.AccessMatrix.ServerName -> Domain.Types.AccessMatrix.UserActionType -> m (Maybe Domain.Types.AccessMatrix.AccessMatrix))
findByRoleIdAndServerAndActionType roleId serverName userActionType = do findOneWithKV [Se.And [Se.Is Beam.roleId $ Se.Eq (Kernel.Types.Id.getId roleId),
                                                                                                Se.Is Beam.serverName $ Se.Eq serverName,
                                                                                                Se.Is Beam.userActionType $ Se.Eq userActionType]]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.AccessMatrix.AccessMatrix -> m (Maybe Domain.Types.AccessMatrix.AccessMatrix))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AccessMatrix.AccessMatrix -> m ())
updateByPrimaryKey (Domain.Types.AccessMatrix.AccessMatrix {..}) = do {_now <- getCurrentTime;
                                                                       updateWithKV [Se.Set Beam.roleId (Kernel.Types.Id.getId roleId),
                                                                                     Se.Set Beam.serverName serverName,
                                                                                     Se.Set Beam.updatedAt _now,
                                                                                     Se.Set Beam.userActionType userActionType] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



instance FromTType' Beam.AccessMatrix Domain.Types.AccessMatrix.AccessMatrix
    where fromTType' (Beam.AccessMatrixT {..}) = do pure $ Just Domain.Types.AccessMatrix.AccessMatrix{createdAt = createdAt,
                                                                                                       id = Kernel.Types.Id.Id id,
                                                                                                       roleId = Kernel.Types.Id.Id roleId,
                                                                                                       serverName = serverName,
                                                                                                       updatedAt = updatedAt,
                                                                                                       userActionType = userActionType}
instance ToTType' Beam.AccessMatrix Domain.Types.AccessMatrix.AccessMatrix
    where toTType' (Domain.Types.AccessMatrix.AccessMatrix {..}) = do Beam.AccessMatrixT{Beam.createdAt = createdAt,
                                                                                         Beam.id = Kernel.Types.Id.getId id,
                                                                                         Beam.roleId = Kernel.Types.Id.getId roleId,
                                                                                         Beam.serverName = serverName,
                                                                                         Beam.updatedAt = updatedAt,
                                                                                         Beam.userActionType = userActionType}



