{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FareAdjustment where

import qualified Domain.Types.FareAdjustment
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Text
import qualified Sequelize as Se
import qualified Storage.Beam.FareAdjustment as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FareAdjustment.FareAdjustment -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FareAdjustment.FareAdjustment] -> m ())
createMany = traverse_ create

findAllByCityAndStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Domain.Types.FareAdjustment.FareAdjustmentStatus -> m [Domain.Types.FareAdjustment.FareAdjustment])
findAllByCityAndStatus merchantOperatingCityId status = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.status $ Se.Eq status
        ]
    ]

findAllByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.FareAdjustment.FareAdjustment])
findAllByMerchantOperatingCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

updateStatusById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FareAdjustment.FareAdjustmentStatus -> Kernel.Types.Id.Id Domain.Types.FareAdjustment.FareAdjustment -> m ())
updateStatusById status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FareAdjustment.FareAdjustment -> m (Maybe Domain.Types.FareAdjustment.FareAdjustment))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FareAdjustment.FareAdjustment -> m ())
updateByPrimaryKey (Domain.Types.FareAdjustment.FareAdjustment {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.areas (Kernel.Utils.Text.encodeToText <$> areas),
      Se.Set Beam.baseFareScalePct baseFareScalePct,
      Se.Set Beam.congestionScalePct congestionScalePct,
      Se.Set Beam.createdBy createdBy,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.mode mode,
      Se.Set Beam.perKmRateScalePct perKmRateScalePct,
      Se.Set Beam.perMinRateScalePct perMinRateScalePct,
      Se.Set Beam.reason reason,
      Se.Set Beam.rolloutPercentage rolloutPercentage,
      Se.Set Beam.status status,
      Se.Set Beam.validFrom validFrom,
      Se.Set Beam.validTill validTill,
      Se.Set Beam.vehicleServiceTiers (Kernel.Utils.Text.encodeToText vehicleServiceTiers),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FareAdjustment Domain.Types.FareAdjustment.FareAdjustment where
  fromTType' (Beam.FareAdjustmentT {..}) = do
    pure $
      Just
        Domain.Types.FareAdjustment.FareAdjustment
          { areas = areas >>= Kernel.Utils.Text.decodeFromText,
            baseFareScalePct = baseFareScalePct,
            congestionScalePct = congestionScalePct,
            createdBy = createdBy,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            mode = mode,
            perKmRateScalePct = perKmRateScalePct,
            perMinRateScalePct = perMinRateScalePct,
            reason = reason,
            rolloutPercentage = rolloutPercentage,
            status = status,
            validFrom = validFrom,
            validTill = validTill,
            vehicleServiceTiers = fromMaybe [] (Kernel.Utils.Text.decodeFromText vehicleServiceTiers),
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FareAdjustment Domain.Types.FareAdjustment.FareAdjustment where
  toTType' (Domain.Types.FareAdjustment.FareAdjustment {..}) = do
    Beam.FareAdjustmentT
      { Beam.areas = Kernel.Utils.Text.encodeToText <$> areas,
        Beam.baseFareScalePct = baseFareScalePct,
        Beam.congestionScalePct = congestionScalePct,
        Beam.createdBy = createdBy,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.mode = mode,
        Beam.perKmRateScalePct = perKmRateScalePct,
        Beam.perMinRateScalePct = perMinRateScalePct,
        Beam.reason = reason,
        Beam.rolloutPercentage = rolloutPercentage,
        Beam.status = status,
        Beam.validFrom = validFrom,
        Beam.validTill = validTill,
        Beam.vehicleServiceTiers = Kernel.Utils.Text.encodeToText vehicleServiceTiers,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
