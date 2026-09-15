{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSFleetStats where

import qualified Data.Text
import qualified Domain.Types.FRFSFleetStats
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSFleetStats as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSFleetStats.FRFSFleetStats -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSFleetStats.FRFSFleetStats] -> m ())
createMany = traverse_ create

findByGtfsIdAndFleetNumber :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> Data.Text.Text -> m (Maybe Domain.Types.FRFSFleetStats.FRFSFleetStats))
findByGtfsIdAndFleetNumber gtfsId fleetNumber = do findOneWithKV [Se.And [Se.Is Beam.gtfsId $ Se.Eq gtfsId, Se.Is Beam.fleetNumber $ Se.Eq fleetNumber]]

updateRatingAgg ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Int -> Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal -> Kernel.Types.Id.Id Domain.Types.FRFSFleetStats.FRFSFleetStats -> m ())
updateRatingAgg totalRatingScore totalRatingCount rating id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.totalRatingScore totalRatingScore,
      Se.Set Beam.totalRatingCount totalRatingCount,
      Se.Set Beam.rating rating,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSFleetStats.FRFSFleetStats -> m (Maybe Domain.Types.FRFSFleetStats.FRFSFleetStats))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSFleetStats.FRFSFleetStats -> m ())
updateByPrimaryKey (Domain.Types.FRFSFleetStats.FRFSFleetStats {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.fleetNumber fleetNumber,
      Se.Set Beam.gtfsId gtfsId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.rating rating,
      Se.Set Beam.totalRatingCount totalRatingCount,
      Se.Set Beam.totalRatingScore totalRatingScore,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSFleetStats Domain.Types.FRFSFleetStats.FRFSFleetStats where
  fromTType' (Beam.FRFSFleetStatsT {..}) = do
    pure $
      Just
        Domain.Types.FRFSFleetStats.FRFSFleetStats
          { createdAt = createdAt,
            fleetNumber = fleetNumber,
            gtfsId = gtfsId,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            rating = rating,
            totalRatingCount = totalRatingCount,
            totalRatingScore = totalRatingScore,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSFleetStats Domain.Types.FRFSFleetStats.FRFSFleetStats where
  toTType' (Domain.Types.FRFSFleetStats.FRFSFleetStats {..}) = do
    Beam.FRFSFleetStatsT
      { Beam.createdAt = createdAt,
        Beam.fleetNumber = fleetNumber,
        Beam.gtfsId = gtfsId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.rating = rating,
        Beam.totalRatingCount = totalRatingCount,
        Beam.totalRatingScore = totalRatingScore,
        Beam.updatedAt = updatedAt
      }
