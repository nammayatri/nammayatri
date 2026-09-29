{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SharedCabBlameCount (module Storage.Queries.SharedCabBlameCount, module ReExport) where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.SharedCabBlameCount
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SharedCabBlameCount as Beam
import Storage.Queries.SharedCabBlameCountExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedCabBlameCount.SharedCabBlameCount -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SharedCabBlameCount.SharedCabBlameCount] -> m ())
createMany = traverse_ create

findTopByMerchantOperatingCityIdAndSubjectType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.SharedCabBlameCount.BlameSubjectType -> m ([Domain.Types.SharedCabBlameCount.SharedCabBlameCount]))
findTopByMerchantOperatingCityIdAndSubjectType limit offset merchantOperatingCityId subjectType = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.subjectType $ Se.Eq subjectType
        ]
    ]
    (Se.Desc Beam.count)
    limit
    offset

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.SharedCabBlameCount.SharedCabBlameCount -> m (Maybe Domain.Types.SharedCabBlameCount.SharedCabBlameCount))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedCabBlameCount.SharedCabBlameCount -> m ())
updateByPrimaryKey (Domain.Types.SharedCabBlameCount.SharedCabBlameCount {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.count count,
      Se.Set Beam.lastAt lastAt,
      Se.Set Beam.lastBookingId lastBookingId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.subjectId subjectId,
      Se.Set Beam.subjectType subjectType,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
