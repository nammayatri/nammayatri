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
  Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
  Kernel.Prelude.Maybe (Kernel.Prelude.Int) ->
  Maybe Domain.Types.TripTransaction.TripStatus ->
  SortType ->
  m [Domain.Types.TripTransaction.TripTransaction]
findAllTripTransactionByDriverIdStatus driverId mbLimit mbOffset mbStatus sortType = do
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
      [Se.And ([Se.Is BeamT.driverId $ Se.Eq driverId.getId] <> statusFilter)]
      filterSort
      (Just limitVal)
      (Just offsetVal)
  pure transactions

findAllTripTransactionByDriverIdActiveStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  m [Domain.Types.TripTransaction.TripTransaction]
findAllTripTransactionByDriverIdActiveStatus driverId = do
  let limitVal = 1
  let offsetVal = 0
  let statusFilter = [Se.Is BeamT.status $ Se.Eq TRIP_ASSIGNED, Se.Is BeamT.status $ Se.Eq IN_PROGRESS]
  transactions <-
    findAllWithOptionsKV
      [Se.And ([Se.Is BeamT.driverId $ Se.Eq driverId.getId, Se.Or statusFilter])]
      (Se.Desc BeamT.createdAt)
      (Just limitVal)
      (Just offsetVal)
  pure transactions

findAllTripTransactionByDriverIdWithinCreationRange ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Int -> Kernel.Prelude.Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> m ([Domain.Types.TripTransaction.TripTransaction]))
findAllTripTransactionByDriverIdWithinCreationRange limit offset driverId mbFrom mbTo = do
  findAllWithOptionsKV
    [ Se.And
        ( [Se.Is BeamT.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]
            <> [Se.Is BeamT.createdAt $ Se.GreaterThanOrEq (fromJust mbFrom) | isJust mbFrom]
            <> [Se.Is BeamT.createdAt $ Se.LessThanOrEq (fromJust mbTo) | isJust mbTo]
        )
    ]
    (Se.Desc BeamT.createdAt)
    limit
    offset