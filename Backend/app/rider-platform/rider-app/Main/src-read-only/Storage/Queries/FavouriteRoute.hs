{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FavouriteRoute where

import qualified Data.Text
import qualified Data.Time.LocalTime
import qualified Domain.Types.FavouriteRoute
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FavouriteRoute as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FavouriteRoute.FavouriteRoute -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FavouriteRoute.FavouriteRoute] -> m ())
createMany = traverse_ create

findAllActiveByTripId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Domain.Types.FavouriteRoute.FavouriteRouteStatus -> m ([Domain.Types.FavouriteRoute.FavouriteRoute]))
findAllActiveByTripId tripId status = do findAllWithKV [Se.And [Se.Is Beam.tripId $ Se.Eq tripId, Se.Is Beam.status $ Se.Eq status]]

findAllActivePatternByRouteCode ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Domain.Types.FavouriteRoute.FavouriteRouteType -> Domain.Types.FavouriteRoute.FavouriteRouteStatus -> Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay -> Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay -> m ([Domain.Types.FavouriteRoute.FavouriteRoute]))
findAllActivePatternByRouteCode routeCode favouriteType status windowStartTime windowEndTime = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.routeCode $ Se.Eq routeCode,
          Se.Is Beam.favouriteType $ Se.Eq favouriteType,
          Se.Is Beam.status $ Se.Eq status,
          Se.Is Beam.windowStartTime $ Se.LessThanOrEq windowStartTime,
          Se.Is Beam.windowEndTime $ Se.GreaterThanOrEq windowEndTime
        ]
    ]

findAllActivePatternByRouteCodeAndNotifyingStop ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Domain.Types.FavouriteRoute.FavouriteRouteType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Domain.Types.FavouriteRoute.FavouriteRouteStatus -> Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay -> Kernel.Prelude.Maybe Data.Time.LocalTime.TimeOfDay -> m ([Domain.Types.FavouriteRoute.FavouriteRoute]))
findAllActivePatternByRouteCodeAndNotifyingStop routeCode favouriteType notifyingStop status windowStartTime windowEndTime = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.routeCode $ Se.Eq routeCode,
          Se.Is Beam.favouriteType $ Se.Eq favouriteType,
          Se.Is Beam.notifyingStop $ Se.Eq notifyingStop,
          Se.Is Beam.status $ Se.Eq status,
          Se.Is Beam.windowStartTime $ Se.LessThanOrEq windowStartTime,
          Se.Is Beam.windowEndTime $ Se.GreaterThanOrEq windowEndTime
        ]
    ]

findAllByRider ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Domain.Types.FavouriteRoute.FavouriteRouteStatus -> m ([Domain.Types.FavouriteRoute.FavouriteRoute]))
findAllByRider riderId status = do findAllWithKV [Se.And [Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId), Se.Is Beam.status $ Se.Eq status]]

findAllByRiderRouteAndStop ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ([Domain.Types.FavouriteRoute.FavouriteRoute]))
findAllByRiderRouteAndStop riderId routeCode notifyingStop = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId),
          Se.Is Beam.routeCode $ Se.Eq routeCode,
          Se.Is Beam.notifyingStop $ Se.Eq notifyingStop
        ]
    ]

findByIdAndRider ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FavouriteRoute.FavouriteRoute -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.FavouriteRoute.FavouriteRoute))
findByIdAndRider id riderId = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id), Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId)]]

updateStatusById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FavouriteRoute.FavouriteRouteStatus -> Kernel.Types.Id.Id Domain.Types.FavouriteRoute.FavouriteRoute -> m ())
updateStatusById status id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FavouriteRoute.FavouriteRoute -> m (Maybe Domain.Types.FavouriteRoute.FavouriteRoute))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FavouriteRoute.FavouriteRoute -> m ())
updateByPrimaryKey (Domain.Types.FavouriteRoute.FavouriteRoute {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.favouriteType favouriteType,
      Se.Set Beam.fromStopCode fromStopCode,
      Se.Set Beam.integratedBppConfigId (Kernel.Types.Id.getId integratedBppConfigId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.notifyOn notifyOn,
      Se.Set Beam.notifyingStop notifyingStop,
      Se.Set Beam.recurrenceRuleDays (map show recurrenceRuleDays),
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.routeCode routeCode,
      Se.Set Beam.routeShortName routeShortName,
      Se.Set Beam.serviceTierType serviceTierType,
      Se.Set Beam.shouldNotify shouldNotify,
      Se.Set Beam.status status,
      Se.Set Beam.toStopCode toStopCode,
      Se.Set Beam.tripId tripId,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.windowEndTime windowEndTime,
      Se.Set Beam.windowStartTime windowStartTime
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FavouriteRoute Domain.Types.FavouriteRoute.FavouriteRoute where
  fromTType' (Beam.FavouriteRouteT {..}) = do
    pure $
      Just
        Domain.Types.FavouriteRoute.FavouriteRoute
          { createdAt = createdAt,
            favouriteType = favouriteType,
            fromStopCode = fromStopCode,
            id = Kernel.Types.Id.Id id,
            integratedBppConfigId = Kernel.Types.Id.Id integratedBppConfigId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            notifyOn = notifyOn,
            notifyingStop = notifyingStop,
            recurrenceRuleDays = ((read . Data.Text.unpack) <$> recurrenceRuleDays),
            riderId = Kernel.Types.Id.Id riderId,
            routeCode = routeCode,
            routeShortName = routeShortName,
            serviceTierType = serviceTierType,
            shouldNotify = shouldNotify,
            status = status,
            toStopCode = toStopCode,
            tripId = tripId,
            updatedAt = updatedAt,
            vehicleType = vehicleType,
            windowEndTime = windowEndTime,
            windowStartTime = windowStartTime
          }

instance ToTType' Beam.FavouriteRoute Domain.Types.FavouriteRoute.FavouriteRoute where
  toTType' (Domain.Types.FavouriteRoute.FavouriteRoute {..}) = do
    Beam.FavouriteRouteT
      { Beam.createdAt = createdAt,
        Beam.favouriteType = favouriteType,
        Beam.fromStopCode = fromStopCode,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.integratedBppConfigId = Kernel.Types.Id.getId integratedBppConfigId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.notifyOn = notifyOn,
        Beam.notifyingStop = notifyingStop,
        Beam.recurrenceRuleDays = map show recurrenceRuleDays,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.routeCode = routeCode,
        Beam.routeShortName = routeShortName,
        Beam.serviceTierType = serviceTierType,
        Beam.shouldNotify = shouldNotify,
        Beam.status = status,
        Beam.toStopCode = toStopCode,
        Beam.tripId = tripId,
        Beam.updatedAt = updatedAt,
        Beam.vehicleType = vehicleType,
        Beam.windowEndTime = windowEndTime,
        Beam.windowStartTime = windowStartTime
      }
