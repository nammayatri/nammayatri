{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.TDSDistributionRecord where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.TDSDistributionRecord
import qualified Storage.Beam.TDSDistributionRecord as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TDSDistributionRecord.TDSDistributionRecord -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.TDSDistributionRecord.TDSDistributionRecord] -> m ())
createMany = traverse_ create
findAllByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                     (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> m ([Domain.Types.TDSDistributionRecord.TDSDistributionRecord]))
findAllByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId <$> driverId)]
findAllByStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TDSDistributionRecord.TDSDistributionStatus -> m ([Domain.Types.TDSDistributionRecord.TDSDistributionRecord]))
findAllByStatus status = do findAllWithKV [Se.Is Beam.status $ Se.Eq status]
findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
            (Kernel.Types.Id.Id Domain.Types.TDSDistributionRecord.TDSDistributionRecord -> m (Maybe Domain.Types.TDSDistributionRecord.TDSDistributionRecord))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                (Domain.Types.TDSDistributionRecord.TDSDistributionStatus -> Kernel.Types.Id.Id Domain.Types.TDSDistributionRecord.TDSDistributionRecord -> m ())
updateStatus status id = do {_now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]}
updateStatusAndRetryCount :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                             (Domain.Types.TDSDistributionRecord.TDSDistributionStatus -> Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.TDSDistributionRecord.TDSDistributionRecord -> m ())
updateStatusAndRetryCount status retryCount id = do {_now <- getCurrentTime;
                                                     updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.retryCount retryCount, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]}
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                    (Kernel.Types.Id.Id Domain.Types.TDSDistributionRecord.TDSDistributionRecord -> m (Maybe Domain.Types.TDSDistributionRecord.TDSDistributionRecord))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TDSDistributionRecord.TDSDistributionRecord -> m ())
updateByPrimaryKey (Domain.Types.TDSDistributionRecord.TDSDistributionRecord {..}) = do {_now <- getCurrentTime;
                                                                                         updateWithKV [Se.Set Beam.assessmentYear assessmentYear,
                                                                                                       Se.Set Beam.driverId (Kernel.Types.Id.getId <$> driverId),
                                                                                                       Se.Set Beam.emailAddress emailAddress,
                                                                                                       Se.Set Beam.fileName fileName,
                                                                                                       Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
                                                                                                       Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
                                                                                                       Se.Set Beam.quarter quarter,
                                                                                                       Se.Set Beam.retryCount retryCount,
                                                                                                       Se.Set Beam.status status,
                                                                                                       Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]}



instance FromTType' Beam.TDSDistributionRecord Domain.Types.TDSDistributionRecord.TDSDistributionRecord
    where fromTType' (Beam.TDSDistributionRecordT {..}) = do pure $ Just Domain.Types.TDSDistributionRecord.TDSDistributionRecord{assessmentYear = assessmentYear,
                                                                                                                                  createdAt = createdAt,
                                                                                                                                  driverId = Kernel.Types.Id.Id <$> driverId,
                                                                                                                                  emailAddress = emailAddress,
                                                                                                                                  fileName = fileName,
                                                                                                                                  id = Kernel.Types.Id.Id id,
                                                                                                                                  merchantId = Kernel.Types.Id.Id merchantId,
                                                                                                                                  merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                                                                  quarter = quarter,
                                                                                                                                  retryCount = retryCount,
                                                                                                                                  status = status,
                                                                                                                                  updatedAt = updatedAt}
instance ToTType' Beam.TDSDistributionRecord Domain.Types.TDSDistributionRecord.TDSDistributionRecord
    where toTType' (Domain.Types.TDSDistributionRecord.TDSDistributionRecord {..}) = do Beam.TDSDistributionRecordT{Beam.assessmentYear = assessmentYear,
                                                                                                                    Beam.createdAt = createdAt,
                                                                                                                    Beam.driverId = Kernel.Types.Id.getId <$> driverId,
                                                                                                                    Beam.emailAddress = emailAddress,
                                                                                                                    Beam.fileName = fileName,
                                                                                                                    Beam.id = Kernel.Types.Id.getId id,
                                                                                                                    Beam.merchantId = Kernel.Types.Id.getId merchantId,
                                                                                                                    Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                                                                    Beam.quarter = quarter,
                                                                                                                    Beam.retryCount = retryCount,
                                                                                                                    Beam.status = status,
                                                                                                                    Beam.updatedAt = updatedAt}



