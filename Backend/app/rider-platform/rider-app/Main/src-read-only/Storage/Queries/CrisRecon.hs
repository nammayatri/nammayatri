{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.CrisRecon where

import qualified Domain.Types.CrisRecon
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.CrisRecon as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CrisRecon.CrisRecon -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.CrisRecon.CrisRecon] -> m ())
createMany = traverse_ create

findByBppOrderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.CrisRecon.CrisRecon))
findByBppOrderId bppOrderId = do findOneWithKV [Se.And [Se.Is Beam.bppOrderId $ Se.Eq bppOrderId]]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.CrisRecon.CrisRecon -> m (Maybe Domain.Types.CrisRecon.CrisRecon))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.CrisRecon.CrisRecon -> m ())
updateByPrimaryKey (Domain.Types.CrisRecon.CrisRecon {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bppOrderId bppOrderId,
      Se.Set Beam.dateIst dateIst,
      Se.Set Beam.fareAmount fareAmount,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.CrisRecon Domain.Types.CrisRecon.CrisRecon where
  fromTType' (Beam.CrisReconT {..}) = do
    pure $
      Just
        Domain.Types.CrisRecon.CrisRecon
          { bppOrderId = bppOrderId,
            createdAt = createdAt,
            dateIst = dateIst,
            fareAmount = fareAmount,
            id = Kernel.Types.Id.Id id,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.CrisRecon Domain.Types.CrisRecon.CrisRecon where
  toTType' (Domain.Types.CrisRecon.CrisRecon {..}) = do
    Beam.CrisReconT
      { Beam.bppOrderId = bppOrderId,
        Beam.createdAt = createdAt,
        Beam.dateIst = dateIst,
        Beam.fareAmount = fareAmount,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
