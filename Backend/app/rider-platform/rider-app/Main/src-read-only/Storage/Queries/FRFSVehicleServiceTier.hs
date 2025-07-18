{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSVehicleServiceTier where

import qualified BecknV2.FRFS.Enums
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
import qualified Storage.Beam.FRFSVehicleServiceTier as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier] -> m ())
createMany = traverse_ create

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier -> m (Maybe Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByProviderCode ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ([Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier]))
findByProviderCode providerCode merchantOperatingCityId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.providerCode $ Se.Eq providerCode,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)
        ]
    ]

findByServiceTierAndMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (BecknV2.FRFS.Enums.ServiceTierType -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier))
findByServiceTierAndMerchantOperatingCityId _type merchantOperatingCityId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam._type $ Se.Eq _type,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier -> m (Maybe Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier -> m ())
updateByPrimaryKey (Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam._type _type,
      Se.Set Beam.description description,
      Se.Set Beam.longName longName,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.providerCode providerCode,
      Se.Set Beam.shortName shortName,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSVehicleServiceTier Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier where
  fromTType' (Beam.FRFSVehicleServiceTierT {..}) = do
    pure $
      Just
        Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier
          { _type = _type,
            description = description,
            id = Kernel.Types.Id.Id id,
            longName = longName,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            providerCode = providerCode,
            shortName = shortName,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSVehicleServiceTier Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier where
  toTType' (Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier {..}) = do
    Beam.FRFSVehicleServiceTierT
      { Beam._type = _type,
        Beam.description = description,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.longName = longName,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.providerCode = providerCode,
        Beam.shortName = shortName,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
