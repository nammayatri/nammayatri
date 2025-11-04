{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BlockedRoute where

import qualified Domain.Types.BlockedRoute
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BlockedRoute as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BlockedRoute.BlockedRoute -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.BlockedRoute.BlockedRoute] -> m ())
createMany = traverse_ create

findAllBlockedRoutesByMerchantOperatingCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> m [Domain.Types.BlockedRoute.BlockedRoute])
findAllBlockedRoutesByMerchantOperatingCity merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BlockedRoute.BlockedRoute -> m (Maybe Domain.Types.BlockedRoute.BlockedRoute))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BlockedRoute.BlockedRoute -> m ())
updateByPrimaryKey (Domain.Types.BlockedRoute.BlockedRoute {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.enabled enabled,
      Se.Set Beam.endSegment endSegment,
      Se.Set Beam.name name,
      Se.Set Beam.startSegment startSegment,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.BlockedRoute Domain.Types.BlockedRoute.BlockedRoute where
  fromTType' (Beam.BlockedRouteT {..}) = do
    pure $
      Just
        Domain.Types.BlockedRoute.BlockedRoute
          { createdAt = createdAt,
            enabled = enabled,
            endSegment = endSegment,
            id = Kernel.Types.Id.Id id,
            name = name,
            startSegment = startSegment,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.BlockedRoute Domain.Types.BlockedRoute.BlockedRoute where
  toTType' (Domain.Types.BlockedRoute.BlockedRoute {..}) = do
    Beam.BlockedRouteT
      { Beam.createdAt = createdAt,
        Beam.enabled = enabled,
        Beam.endSegment = endSegment,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.name = name,
        Beam.startSegment = startSegment,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
