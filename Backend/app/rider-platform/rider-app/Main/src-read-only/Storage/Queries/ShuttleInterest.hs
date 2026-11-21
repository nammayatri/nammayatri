{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ShuttleInterest where

import qualified Data.Text
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.ShuttleInterest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.ShuttleInterest as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.ShuttleInterest.ShuttleInterest -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.ShuttleInterest.ShuttleInterest] -> m ()
createMany = traverse_ create

findAllByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Int ->
  (Maybe Int -> (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.ShuttleInterest.ShuttleInterest]))
findAllByMerchantOperatingCityId limit offset merchantOperatingCityId = do findAllWithOptionsKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)] (Se.Desc Beam.updatedAt) limit offset

findAllByMerchantOperatingCityIdAndCompanyDomain ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Int ->
  (Maybe Int -> (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> (Kernel.Prelude.Text -> m [Domain.Types.ShuttleInterest.ShuttleInterest])))
findAllByMerchantOperatingCityIdAndCompanyDomain limit offset merchantOperatingCityId companyDomain = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.companyDomain $ Se.Eq companyDomain
        ]
    ]
    (Se.Desc Beam.updatedAt)
    limit
    offset

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.ShuttleInterest.ShuttleInterest)
findByPrimaryKey personId = do findOneWithKV [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.ShuttleInterest.ShuttleInterest -> m ()
updateByPrimaryKey (Domain.Types.ShuttleInterest.ShuttleInterest {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.acPreference acPreference,
      Se.Set Beam.companyDomain companyDomain,
      Se.Set Beam.daysOfWeek (map show daysOfWeek),
      Se.Set Beam.dropAddress dropAddress,
      Se.Set Beam.dropLat dropLat,
      Se.Set Beam.dropLon dropLon,
      Se.Set Beam.dropTime dropTime,
      Se.Set Beam.maxWalkingDistanceMeters maxWalkingDistanceMeters,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.pickupAddress pickupAddress,
      Se.Set Beam.pickupLat pickupLat,
      Se.Set Beam.pickupLon pickupLon,
      Se.Set Beam.pickupTime pickupTime,
      Se.Set Beam.shiftEndTime shiftEndTime,
      Se.Set Beam.shiftStartTime shiftStartTime,
      Se.Set Beam.timeFlexibilityMinutes timeFlexibilityMinutes,
      Se.Set Beam.tripType tripType,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleSizePreference vehicleSizePreference
    ]
    [Se.And [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]]

instance FromTType' Beam.ShuttleInterest Domain.Types.ShuttleInterest.ShuttleInterest where
  fromTType' (Beam.ShuttleInterestT {..}) = do
    pure $
      Just
        Domain.Types.ShuttleInterest.ShuttleInterest
          { acPreference = acPreference,
            companyDomain = companyDomain,
            createdAt = createdAt,
            daysOfWeek = ((read . Data.Text.unpack) <$> daysOfWeek),
            dropAddress = dropAddress,
            dropLat = dropLat,
            dropLon = dropLon,
            dropTime = dropTime,
            maxWalkingDistanceMeters = maxWalkingDistanceMeters,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            personId = Kernel.Types.Id.Id personId,
            pickupAddress = pickupAddress,
            pickupLat = pickupLat,
            pickupLon = pickupLon,
            pickupTime = pickupTime,
            shiftEndTime = shiftEndTime,
            shiftStartTime = shiftStartTime,
            timeFlexibilityMinutes = timeFlexibilityMinutes,
            tripType = tripType,
            updatedAt = updatedAt,
            vehicleSizePreference = vehicleSizePreference
          }

instance ToTType' Beam.ShuttleInterest Domain.Types.ShuttleInterest.ShuttleInterest where
  toTType' (Domain.Types.ShuttleInterest.ShuttleInterest {..}) = do
    Beam.ShuttleInterestT
      { Beam.acPreference = acPreference,
        Beam.companyDomain = companyDomain,
        Beam.createdAt = createdAt,
        Beam.daysOfWeek = map show daysOfWeek,
        Beam.dropAddress = dropAddress,
        Beam.dropLat = dropLat,
        Beam.dropLon = dropLon,
        Beam.dropTime = dropTime,
        Beam.maxWalkingDistanceMeters = maxWalkingDistanceMeters,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.pickupAddress = pickupAddress,
        Beam.pickupLat = pickupLat,
        Beam.pickupLon = pickupLon,
        Beam.pickupTime = pickupTime,
        Beam.shiftEndTime = shiftEndTime,
        Beam.shiftStartTime = shiftStartTime,
        Beam.timeFlexibilityMinutes = timeFlexibilityMinutes,
        Beam.tripType = tripType,
        Beam.updatedAt = updatedAt,
        Beam.vehicleSizePreference = vehicleSizePreference
      }
