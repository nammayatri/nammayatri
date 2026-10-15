{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RouteStopCalender where

import qualified Domain.Types.IntegratedBPPConfig
import qualified Domain.Types.RouteStopCalender
import qualified Domain.Types.RouteStopTimeTable
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RouteStopCalender as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RouteStopCalender.RouteStopCalender -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RouteStopCalender.RouteStopCalender] -> m ())
createMany = traverse_ create

findByTripIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  ([Kernel.Types.Id.Id Domain.Types.RouteStopTimeTable.RouteStopTimeTable] -> Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> m [Domain.Types.RouteStopCalender.RouteStopCalender])
findByTripIds tripId integratedBppConfigId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.tripId $ Se.In (Kernel.Types.Id.getId <$> tripId),
          Se.Is Beam.integratedBppConfigId $ Se.Eq (Kernel.Types.Id.getId integratedBppConfigId)
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig -> Kernel.Types.Id.Id Domain.Types.RouteStopTimeTable.RouteStopTimeTable -> m (Maybe Domain.Types.RouteStopCalender.RouteStopCalender))
findByPrimaryKey integratedBppConfigId tripId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.integratedBppConfigId $ Se.Eq (Kernel.Types.Id.getId integratedBppConfigId),
          Se.Is Beam.tripId $ Se.Eq (Kernel.Types.Id.getId tripId)
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RouteStopCalender.RouteStopCalender -> m ())
updateByPrimaryKey (Domain.Types.RouteStopCalender.RouteStopCalender {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.serviceability serviceability,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.integratedBppConfigId $ Se.Eq (Kernel.Types.Id.getId integratedBppConfigId), Se.Is Beam.tripId $ Se.Eq (Kernel.Types.Id.getId tripId)]]

instance FromTType' Beam.RouteStopCalender Domain.Types.RouteStopCalender.RouteStopCalender where
  fromTType' (Beam.RouteStopCalenderT {..}) = do
    pure $
      Just
        Domain.Types.RouteStopCalender.RouteStopCalender
          { integratedBppConfigId = Kernel.Types.Id.Id integratedBppConfigId,
            serviceability = serviceability,
            tripId = Kernel.Types.Id.Id tripId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RouteStopCalender Domain.Types.RouteStopCalender.RouteStopCalender where
  toTType' (Domain.Types.RouteStopCalender.RouteStopCalender {..}) = do
    Beam.RouteStopCalenderT
      { Beam.integratedBppConfigId = Kernel.Types.Id.getId integratedBppConfigId,
        Beam.serviceability = serviceability,
        Beam.tripId = Kernel.Types.Id.getId tripId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
