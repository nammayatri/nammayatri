{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.IncentiveJourney where

import qualified Domain.Types.IncentiveJourney
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.IncentiveJourney as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.IncentiveJourney.IncentiveJourney -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.IncentiveJourney.IncentiveJourney] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.IncentiveJourney.IncentiveJourney -> m (Maybe Domain.Types.IncentiveJourney.IncentiveJourney))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.IncentiveJourney.IncentiveJourney])
findByMerchantOperatingCityId limit offset merchantOperatingCityId = do findAllWithOptionsKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)] (Se.Desc Beam.createdAt) limit offset

findEnabledByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Bool -> m [Domain.Types.IncentiveJourney.IncentiveJourney])
findEnabledByMerchantOperatingCityId limit offset merchantOperatingCityId enabled = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.enabled $ Se.Eq enabled
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.IncentiveJourney.IncentiveJourney -> m (Maybe Domain.Types.IncentiveJourney.IncentiveJourney))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.IncentiveJourney.IncentiveJourney -> m ())
updateByPrimaryKey (Domain.Types.IncentiveJourney.IncentiveJourney {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.description description,
      Se.Set Beam.driverTag driverTag,
      Se.Set Beam.enabled enabled,
      Se.Set Beam.endDate endDate,
      Se.Set Beam.journeyType journeyType,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.name name,
      Se.Set Beam.serviceTierType serviceTierType,
      Se.Set Beam.startDate startDate,
      Se.Set Beam.timeBounds timeBounds,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleCategory vehicleCategory
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.IncentiveJourney Domain.Types.IncentiveJourney.IncentiveJourney where
  fromTType' (Beam.IncentiveJourneyT {..}) = do
    pure $
      Just
        Domain.Types.IncentiveJourney.IncentiveJourney
          { createdAt = createdAt,
            description = description,
            driverTag = driverTag,
            enabled = enabled,
            endDate = endDate,
            id = Kernel.Types.Id.Id id,
            journeyType = journeyType,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            serviceTierType = serviceTierType,
            startDate = startDate,
            timeBounds = timeBounds,
            updatedAt = updatedAt,
            vehicleCategory = vehicleCategory
          }

instance ToTType' Beam.IncentiveJourney Domain.Types.IncentiveJourney.IncentiveJourney where
  toTType' (Domain.Types.IncentiveJourney.IncentiveJourney {..}) = do
    Beam.IncentiveJourneyT
      { Beam.createdAt = createdAt,
        Beam.description = description,
        Beam.driverTag = driverTag,
        Beam.enabled = enabled,
        Beam.endDate = endDate,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.journeyType = journeyType,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.serviceTierType = serviceTierType,
        Beam.startDate = startDate,
        Beam.timeBounds = timeBounds,
        Beam.updatedAt = updatedAt,
        Beam.vehicleCategory = vehicleCategory
      }
