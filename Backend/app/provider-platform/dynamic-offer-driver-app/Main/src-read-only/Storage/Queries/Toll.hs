{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Toll where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Toll
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Toll as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Toll.Toll -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Toll.Toll] -> m ())
createMany = traverse_ create

findAllTollsByMerchantOperatingCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> m [Domain.Types.Toll.Toll])
findAllTollsByMerchantOperatingCity merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Toll.Toll -> m (Maybe Domain.Types.Toll.Toll))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Toll.Toll -> m ())
updateByPrimaryKey (Domain.Types.Toll.Toll {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.isAutoRickshawAllowed isAutoRickshawAllowed,
      Se.Set Beam.isTwoWheelerAllowed isTwoWheelerAllowed,
      Se.Set Beam.name name,
      Se.Set Beam.currency ((Kernel.Prelude.Just . (.currency)) price),
      Se.Set Beam.price ((.amount) price),
      Se.Set Beam.tollEndGates tollEndGates,
      Se.Set Beam.tollStartGates tollStartGates,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.Toll Domain.Types.Toll.Toll where
  fromTType' (Beam.TollT {..}) = do
    pure $
      Just
        Domain.Types.Toll.Toll
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            isAutoRickshawAllowed = isAutoRickshawAllowed,
            isTwoWheelerAllowed = isTwoWheelerAllowed,
            name = name,
            price = Kernel.Types.Common.mkPrice currency price,
            tollEndGates = tollEndGates,
            tollStartGates = tollStartGates,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.Toll Domain.Types.Toll.Toll where
  toTType' (Domain.Types.Toll.Toll {..}) = do
    Beam.TollT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isAutoRickshawAllowed = isAutoRickshawAllowed,
        Beam.isTwoWheelerAllowed = isTwoWheelerAllowed,
        Beam.name = name,
        Beam.currency = (Kernel.Prelude.Just . (.currency)) price,
        Beam.price = (.amount) price,
        Beam.tollEndGates = tollEndGates,
        Beam.tollStartGates = tollStartGates,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
