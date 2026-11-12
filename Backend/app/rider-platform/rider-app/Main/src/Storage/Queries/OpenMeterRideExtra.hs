{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OpenMeterRideExtra where

import qualified Domain.Types.MerchantOperatingCity as DMOC
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

-- | A merchant city's rides for the dashboard, newest first: optionally one rider's, one status, and a
-- createdAt window. With a rider it reads through KV like the rider's own list (personId is the
-- secondary key). Without one there is no KV key to read by, so it reads Postgres, and a ride the
-- drainer has not written yet shows up a little later.
findAllByMerchantOpCityIdPaginated ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe (Id DP.Person) ->
  Maybe DOMR.OpenMeterRideStatus ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Int ->
  Int ->
  m [DOMR.OpenMeterRide]
findAllByMerchantOpCityIdPaginated merchantOpCityId mbPersonId mbStatus mbFrom mbTo limit offset =
  case mbPersonId of
    Just _ -> findAllWithOptionsKV conditions (Se.Desc Beam.createdAt) (Just limit) (Just offset)
    Nothing -> findAllWithOptionsDb conditions (Se.Desc Beam.createdAt) (Just limit) (Just offset)
  where
    conditions =
      [ Se.And
          ( [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Just $ getId merchantOpCityId)]
              <> maybe [] (\riderId -> [Se.Is Beam.personId $ Se.Eq (getId riderId)]) mbPersonId
              <> maybe [] (\rideStatus -> [Se.Is Beam.status $ Se.Eq rideStatus]) mbStatus
              <> maybe [] (\fromTime -> [Se.Is Beam.createdAt $ Se.GreaterThanOrEq fromTime]) mbFrom
              <> maybe [] (\toTime -> [Se.Is Beam.createdAt $ Se.LessThanOrEq toTime]) mbTo
          )
      ]
