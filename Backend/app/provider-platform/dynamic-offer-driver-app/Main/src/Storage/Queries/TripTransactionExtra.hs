{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TripTransactionExtra where

import Domain.Types.Person
import Domain.Types.TripTransaction
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.TripTransaction as BeamT
import Storage.Queries.OrphanInstances.TripTransaction

data SortType = SortAsc | SortDesc

findAllTripTransactionByDriverIdStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
  Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
  Maybe Domain.Types.TripTransaction.TripStatus ->
  SortType ->
  m [Domain.Types.TripTransaction.TripTransaction]
findAllTripTransactionByDriverIdStatus fleetOwnerId driverId mbLimit mbOffset mbStatus sortType = do
  let limitVal = case mbLimit of
        Just val -> val
        Nothing -> 10
  let offsetVal = case mbOffset of
        Just val -> val
        Nothing -> 0
  let statusFilter =
        case mbStatus of
          Just status -> [Se.Is BeamT.status $ Se.Eq status]
          Nothing -> []
  let filterSort =
        case sortType of
          SortAsc -> Se.Asc BeamT.createdAt
          SortDesc -> Se.Desc BeamT.createdAt
  transactions <-
    findAllWithOptionsKV
      [Se.And ([Se.Is BeamT.driverId $ Se.Eq driverId.getId, Se.Is BeamT.fleetOwnerId $ Se.Eq fleetOwnerId.getId] <> statusFilter)]
      filterSort
      (Just limitVal)
      (Just offsetVal)
  pure transactions

findAllTripTransactionByDriverIdStatusForPilot ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
  Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
  Maybe Domain.Types.TripTransaction.TripStatus ->
  SortType ->
  m [Domain.Types.TripTransaction.TripTransaction]
findAllTripTransactionByDriverIdStatusForPilot fleetOwnerId driverId mbLimit mbOffset mbStatus sortType = do
  let limitVal = case mbLimit of
        Just val -> val
        Nothing -> 10
  let offsetVal = case mbOffset of
        Just val -> val
        Nothing -> 0
  let statusFilter =
        case mbStatus of
          Just status -> [Se.Is BeamT.status $ Se.Eq status]
          Nothing -> []
  let filterSort =
        case sortType of
          SortAsc -> Se.Asc BeamT.scheduledTripTime
          SortDesc -> Se.Desc BeamT.scheduledTripTime
  transactions <-
    findAllWithOptionsKV
      [Se.And ([Se.Is BeamT.driverId $ Se.Eq driverId.getId, Se.Is BeamT.fleetOwnerId $ Se.Eq fleetOwnerId.getId, Se.Is BeamT.tripType $ Se.Eq (Just PILOT)] <> statusFilter)]
      filterSort
      (Just limitVal)
      (Just offsetVal)
  pure transactions

findAllTripTransactionByDriverIdActiveStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  m [Domain.Types.TripTransaction.TripTransaction]
findAllTripTransactionByDriverIdActiveStatus fleetOwnerId mbLimit driverId = do
  let limitVal = case mbLimit of
        Just val -> val
        Nothing -> 10
  let offsetVal = 0
  let statusFilter = [Se.Is BeamT.status $ Se.Eq TRIP_ASSIGNED, Se.Is BeamT.status $ Se.Eq IN_PROGRESS]
  transactions <-
    findAllWithOptionsKV
      [Se.And ([Se.Is BeamT.driverId $ Se.Eq driverId.getId, Se.Is BeamT.fleetOwnerId $ Se.Eq fleetOwnerId.getId, Se.Or statusFilter])]
      (Se.Desc BeamT.createdAt)
      (Just limitVal)
      (Just offsetVal)
  pure transactions

findAllTripTransactionByDriverIdWithinCreationRange ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Int -> Kernel.Prelude.Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ([Domain.Types.TripTransaction.TripTransaction]))
findAllTripTransactionByDriverIdWithinCreationRange fleetOwnerId limit offset driverId mbFrom mbTo mbVehicleNumber = do
  findAllWithOptionsKV
    [ Se.And
        ( [Se.Is BeamT.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is BeamT.fleetOwnerId $ Se.Eq fleetOwnerId.getId]
            <> [Se.Is BeamT.createdAt $ Se.GreaterThanOrEq (fromJust mbFrom) | isJust mbFrom]
            <> [Se.Is BeamT.createdAt $ Se.LessThanOrEq (fromJust mbTo) | isJust mbTo]
            <> [Se.Is BeamT.vehicleNumber $ Se.Eq (fromJust mbVehicleNumber) | isJust mbVehicleNumber]
        )
    ]
    (Se.Desc BeamT.createdAt)
    limit
    offset

findAllTripTransactionByDriverIdWithinCreationRangeMultiFleetOwner ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  ([Text] -> Kernel.Prelude.Maybe Int -> Kernel.Prelude.Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ([Domain.Types.TripTransaction.TripTransaction]))
findAllTripTransactionByDriverIdWithinCreationRangeMultiFleetOwner fleetOwnerIds limit offset driverId mbFrom mbTo mbVehicleNumber = do
  findAllWithOptionsKV
    [ Se.And
        ( [Se.Is BeamT.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is BeamT.fleetOwnerId $ Se.In fleetOwnerIds]
            <> [Se.Is BeamT.createdAt $ Se.GreaterThanOrEq (fromJust mbFrom) | isJust mbFrom]
            <> [Se.Is BeamT.createdAt $ Se.LessThanOrEq (fromJust mbTo) | isJust mbTo]
            <> [Se.Is BeamT.vehicleNumber $ Se.Eq (fromJust mbVehicleNumber) | isJust mbVehicleNumber]
        )
    ]
    (Se.Desc BeamT.createdAt)
    limit
    offset

findAllTripTransactionByFleetOwnerIdAndTripType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  ([Text] -> Domain.Types.TripTransaction.TripType -> Kernel.Prelude.Maybe Int -> Kernel.Prelude.Maybe Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Domain.Types.TripTransaction.TripStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ([Domain.Types.TripTransaction.TripTransaction]))
findAllTripTransactionByFleetOwnerIdAndTripType fleetOwnerIds tripType limit offset mbDriverId mbFrom mbTo mbStatus mbVehicleNumber mbDutyType = do
  findAllWithOptionsKV
    [ Se.And
        ( [Se.Is BeamT.fleetOwnerId $ Se.In fleetOwnerIds, Se.Is BeamT.tripType $ Se.Eq (Just tripType)]
            <> [Se.Is BeamT.driverId $ Se.Eq (Kernel.Types.Id.getId (fromJust mbDriverId)) | isJust mbDriverId]
            <> [Se.Is BeamT.createdAt $ Se.GreaterThanOrEq (fromJust mbFrom) | isJust mbFrom]
            <> [Se.Is BeamT.createdAt $ Se.LessThanOrEq (fromJust mbTo) | isJust mbTo]
            <> [Se.Is BeamT.status $ Se.Eq (fromJust mbStatus) | isJust mbStatus]
            <> [Se.Is BeamT.vehicleNumber $ Se.Eq (fromJust mbVehicleNumber) | isJust mbVehicleNumber]
            <> [Se.Is BeamT.dutyType $ Se.Eq (Just (fromJust mbDutyType)) | isJust mbDutyType]
        )
    ]
    (Se.Desc BeamT.createdAt)
    limit
    offset
