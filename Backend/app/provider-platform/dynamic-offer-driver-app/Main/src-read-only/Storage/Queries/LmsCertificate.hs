{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.LmsCertificate where

import qualified Domain.Types.LmsCertificate
import qualified Domain.Types.LmsModule
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.LmsCertificate as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.LmsCertificate.LmsCertificate -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.LmsCertificate.LmsCertificate] -> m ())
createMany = traverse_ create

findByModuleCompletionIdAndDriverIdAndModuleId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule -> m (Maybe Domain.Types.LmsCertificate.LmsCertificate))
findByModuleCompletionIdAndDriverIdAndModuleId moduleCompletionId driverId moduleId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.moduleCompletionId $ Se.Eq moduleCompletionId,
          Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.moduleId $ Se.Eq (Kernel.Types.Id.getId moduleId)
        ]
    ]

getAllCertificate :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.LmsCertificate.LmsCertificate])
getAllCertificate driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.LmsCertificate.LmsCertificate -> m (Maybe Domain.Types.LmsCertificate.LmsCertificate))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.LmsCertificate.LmsCertificate -> m ())
updateByPrimaryKey (Domain.Types.LmsCertificate.LmsCertificate {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.moduleCompletionId moduleCompletionId,
      Se.Set Beam.moduleId (Kernel.Types.Id.getId moduleId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.LmsCertificate Domain.Types.LmsCertificate.LmsCertificate where
  fromTType' (Beam.LmsCertificateT {..}) = do
    pure $
      Just
        Domain.Types.LmsCertificate.LmsCertificate
          { driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            moduleCompletionId = moduleCompletionId,
            moduleId = Kernel.Types.Id.Id moduleId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.LmsCertificate Domain.Types.LmsCertificate.LmsCertificate where
  toTType' (Domain.Types.LmsCertificate.LmsCertificate {..}) = do
    Beam.LmsCertificateT
      { Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.moduleCompletionId = moduleCompletionId,
        Beam.moduleId = Kernel.Types.Id.getId moduleId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
