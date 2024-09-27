{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RouteStopFare where

import qualified Domain.Types.RouteStopFare
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RouteStopFare as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RouteStopFare.RouteStopFare -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RouteStopFare.RouteStopFare] -> m ())
createMany = traverse_ create

findByRouteStartAndStopCode :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Domain.Types.RouteStopFare.RouteStopFare))
findByRouteStartAndStopCode routeCode startStopCode endStopCode = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.routeCode $ Se.Eq routeCode,
          Se.Is Beam.startStopCode $ Se.Eq startStopCode,
          Se.Is Beam.endStopCode $ Se.Eq endStopCode
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe Domain.Types.RouteStopFare.RouteStopFare))
findByPrimaryKey endStopCode routeCode startStopCode = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.endStopCode $ Se.Eq endStopCode,
          Se.Is Beam.routeCode $ Se.Eq routeCode,
          Se.Is Beam.startStopCode $ Se.Eq startStopCode
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RouteStopFare.RouteStopFare -> m ())
updateByPrimaryKey (Domain.Types.RouteStopFare.RouteStopFare {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.currency currency,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.endStopCode $ Se.Eq endStopCode, Se.Is Beam.routeCode $ Se.Eq routeCode, Se.Is Beam.startStopCode $ Se.Eq startStopCode]]

instance FromTType' Beam.RouteStopFare Domain.Types.RouteStopFare.RouteStopFare where
  fromTType' (Beam.RouteStopFareT {..}) = do
    pure $
      Just
        Domain.Types.RouteStopFare.RouteStopFare
          { amount = amount,
            currency = currency,
            endStopCode = endStopCode,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            routeCode = routeCode,
            startStopCode = startStopCode,
            vehicleType = vehicleType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RouteStopFare Domain.Types.RouteStopFare.RouteStopFare where
  toTType' (Domain.Types.RouteStopFare.RouteStopFare {..}) = do
    Beam.RouteStopFareT
      { Beam.amount = amount,
        Beam.currency = currency,
        Beam.endStopCode = endStopCode,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.routeCode = routeCode,
        Beam.startStopCode = startStopCode,
        Beam.vehicleType = vehicleType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
