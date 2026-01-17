{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSGtfsStageFare where

import qualified BecknV2.FRFS.Enums
import qualified Domain.Types.FRFSGtfsStageFare
import qualified Domain.Types.FRFSVehicleServiceTier
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSGtfsStageFare as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSGtfsStageFare.FRFSGtfsStageFare -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSGtfsStageFare.FRFSGtfsStageFare] -> m ())
createMany = traverse_ create

findAllByVehicleServiceTierId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier -> m [Domain.Types.FRFSGtfsStageFare.FRFSGtfsStageFare])
findAllByVehicleServiceTierId vehicleServiceTierId = do findAllWithKV [Se.Is Beam.vehicleServiceTierId $ Se.Eq (Kernel.Types.Id.getId vehicleServiceTierId)]

findAllByVehicleTypeAndStageAndMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (BecknV2.FRFS.Enums.VehicleCategory -> Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.FRFSGtfsStageFare.FRFSGtfsStageFare])
findAllByVehicleTypeAndStageAndMerchantOperatingCityId vehicleType stage merchantOperatingCityId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.vehicleType $ Se.Eq vehicleType,
          Se.Is Beam.stage $ Se.Eq stage,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)
        ]
    ]

findOneByVehicleTypeAndStageAndMerchantOperatingCityIdAndVehicleServiceTierId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (BecknV2.FRFS.Enums.VehicleCategory -> Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Types.Id.Id Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier -> m (Maybe Domain.Types.FRFSGtfsStageFare.FRFSGtfsStageFare))
findOneByVehicleTypeAndStageAndMerchantOperatingCityIdAndVehicleServiceTierId vehicleType stage merchantOperatingCityId vehicleServiceTierId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.vehicleType $ Se.Eq vehicleType,
          Se.Is Beam.stage $ Se.Eq stage,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.vehicleServiceTierId $ Se.Eq (Kernel.Types.Id.getId vehicleServiceTierId)
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSGtfsStageFare.FRFSGtfsStageFare -> m (Maybe Domain.Types.FRFSGtfsStageFare.FRFSGtfsStageFare))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSGtfsStageFare.FRFSGtfsStageFare -> m ())
updateByPrimaryKey (Domain.Types.FRFSGtfsStageFare.FRFSGtfsStageFare {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.cessCharge cessCharge,
      Se.Set Beam.currency currency,
      Se.Set Beam.discountIds (Kernel.Types.Id.getId <$> discountIds),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.stage stage,
      Se.Set Beam.vehicleServiceTierId (Kernel.Types.Id.getId vehicleServiceTierId),
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSGtfsStageFare Domain.Types.FRFSGtfsStageFare.FRFSGtfsStageFare where
  fromTType' (Beam.FRFSGtfsStageFareT {..}) = do
    pure $
      Just
        Domain.Types.FRFSGtfsStageFare.FRFSGtfsStageFare
          { amount = amount,
            cessCharge = cessCharge,
            currency = currency,
            discountIds = Kernel.Types.Id.Id <$> discountIds,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            stage = stage,
            vehicleServiceTierId = Kernel.Types.Id.Id vehicleServiceTierId,
            vehicleType = vehicleType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSGtfsStageFare Domain.Types.FRFSGtfsStageFare.FRFSGtfsStageFare where
  toTType' (Domain.Types.FRFSGtfsStageFare.FRFSGtfsStageFare {..}) = do
    Beam.FRFSGtfsStageFareT
      { Beam.amount = amount,
        Beam.cessCharge = cessCharge,
        Beam.currency = currency,
        Beam.discountIds = Kernel.Types.Id.getId <$> discountIds,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.stage = stage,
        Beam.vehicleServiceTierId = Kernel.Types.Id.getId vehicleServiceTierId,
        Beam.vehicleType = vehicleType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
