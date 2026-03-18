{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SavedTripExtra where

import Data.Time hiding (getCurrentTime)
import qualified Domain.Types.Person
import qualified Domain.Types.SavedTrip as DST
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SavedTrip as Beam
import Storage.Queries.OrphanInstances.SavedTrip

-- Extra code goes here --

findAllActiveRecurring :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => m [DST.SavedTrip]
findAllActiveRecurring = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.isActive $ Se.Eq True,
          Se.Is Beam.recurrence $ Se.Not (Se.Eq DST.NoRecurrence)
        ]
    ]

updateLastComputedDeparture ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Prelude.Maybe Kernel.Prelude.UTCTime ->
  Kernel.Types.Id.Id DST.SavedTrip ->
  m ()
updateLastComputedDeparture lastComputedDeparture id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.lastComputedDeparture lastComputedDeparture,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findAllByRiderIdWithLimitOffset ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Int ->
  Int ->
  m [DST.SavedTrip]
findAllByRiderIdWithLimitOffset riderId limitVal offsetVal = do
  findAllWithOptionsKV
    [Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId)]
    (Se.Desc Beam.createdAt)
    (Just limitVal)
    (Just offsetVal)
