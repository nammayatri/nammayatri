{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.GTFSFeedInfo where

import qualified BecknV2.FRFS.Enums
import qualified Domain.Types.GTFSFeedInfo
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.GTFSFeedInfo as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.GTFSFeedInfo.GTFSFeedInfo -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.GTFSFeedInfo.GTFSFeedInfo] -> m ())
createMany = traverse_ create

findByVehicleTypeAndCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (BecknV2.FRFS.Enums.VehicleCategory -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.GTFSFeedInfo.GTFSFeedInfo))
findByVehicleTypeAndCity vehicleType merchantId merchantOperatingCityId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.vehicleType $ Se.Eq vehicleType,
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.GTFSFeedInfo.GTFSFeedInfo -> m (Maybe Domain.Types.GTFSFeedInfo.GTFSFeedInfo))
findByPrimaryKey feedId = do findOneWithKV [Se.And [Se.Is Beam.feedId $ Se.Eq (Kernel.Types.Id.getId feedId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.GTFSFeedInfo.GTFSFeedInfo -> m ())
updateByPrimaryKey (Domain.Types.GTFSFeedInfo.GTFSFeedInfo {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.feedId $ Se.Eq (Kernel.Types.Id.getId feedId)]]

instance FromTType' Beam.GTFSFeedInfo Domain.Types.GTFSFeedInfo.GTFSFeedInfo where
  fromTType' (Beam.GTFSFeedInfoT {..}) = do
    pure $
      Just
        Domain.Types.GTFSFeedInfo.GTFSFeedInfo
          { feedId = Kernel.Types.Id.Id feedId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            vehicleType = vehicleType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.GTFSFeedInfo Domain.Types.GTFSFeedInfo.GTFSFeedInfo where
  toTType' (Domain.Types.GTFSFeedInfo.GTFSFeedInfo {..}) = do
    Beam.GTFSFeedInfoT
      { Beam.feedId = Kernel.Types.Id.getId feedId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.vehicleType = vehicleType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
