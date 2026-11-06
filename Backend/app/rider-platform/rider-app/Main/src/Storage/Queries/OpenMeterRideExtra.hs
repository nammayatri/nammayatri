{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OpenMeterRideExtra where

import qualified Domain.Types.OpenMeterRide as DOMR
import qualified Domain.Types.Person as DP
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.OpenMeterRide as Beam
import Storage.Queries.OrphanInstances.OpenMeterRide

-- Extra code goes here --

defaultOpenMeterRidesLimit :: Int
defaultOpenMeterRidesLimit = 20

maxOpenMeterRidesLimit :: Int
maxOpenMeterRidesLimit = 50

-- | Rides of a rider, newest first. Uses the KV-aware read (not findAllWithDb) because
-- rows that the drainer has not yet written to Postgres would otherwise be missed.
findAllByPersonIdPaginated ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DP.Person ->
  Maybe DOMR.OpenMeterRideStatus ->
  Maybe Int ->
  Maybe Int ->
  m [DOMR.OpenMeterRide]
findAllByPersonIdPaginated personId mbStatus mbLimit mbOffset =
  findAllWithOptionsKV
    [Se.And ([Se.Is Beam.personId $ Se.Eq (getId personId)] <> maybe [] (\status -> [Se.Is Beam.status $ Se.Eq status]) mbStatus)]
    (Se.Desc Beam.createdAt)
    (Just limit)
    (Just offset)
  where
    limit = min maxOpenMeterRidesLimit . max 1 $ fromMaybe defaultOpenMeterRidesLimit mbLimit
    offset = max 0 $ fromMaybe 0 mbOffset
