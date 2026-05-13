{-# LANGUAGE RankNTypes #-}

module SharedLogic.DriverPool.DriverPoolMigrations
  ( Migrator,
    MigrationEntry (..),
    migrations,
    currentSchemaVersion,
    applyMigrations,
  )
where

import qualified Data.HashMap.Strict as HashMap
import Data.List (partition, sortOn)
import qualified Data.List as DL
import qualified Domain.Types.Person as Person
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import SharedLogic.DriverPool.DriverPoolData
import qualified Storage.Queries.DriverBankAccount as QDBA
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.FleetDriverAssociation as QFDA

-- | A migrator takes a batch of LTS-loaded entries and returns the same entries
-- with ONLY the fields it owns rewritten from DB. It MUST NOT touch
-- 'schemaVersion' — 'applyMigrations' stamps that after the migrator returns
-- so a migrator cannot lie about which version its outputs satisfy.
type Migrator m = [DriverPoolData] -> m [DriverPoolData]

-- | One entry in the migrations registry: its target schema version + the
-- migrator function. Using a record (with a Rank-2 field) lets us extract
-- 'meVersion' without picking a monad, so 'currentSchemaVersion' is a plain
-- Int derived from the same registry the runtime walks.
data MigrationEntry = MigrationEntry
  { meVersion :: Int,
    meMigrator ::
      forall m r.
      (BeamFlow m r, MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
      Migrator m
  }

-- | Append-only registry of all schema-version bumps.
--
-- A migrator runs for an entry IFF the entry's stored 'poolDataSchemaVersion'
-- is strictly less than its 'meVersion'. 'applyMigrations' stamps
-- 'schemaVersion = Just meVersion' on each entry the migrator touched.
--
-- ## Iteration semantics
--
--   stored=0  -> runs migrator 1, then migrator 2, then 3, …
--   stored=1  -> skips migrator 1, runs migrator 2, then 3, …
--   stored=N  -> skips everything (no DB hit)
--
-- Adding a future field (e.g. driverTrustScore in v2):
--   1. Add the field to DriverPoolData / defaultDriverPoolData / buildDriverPoolDataFromDB.
--   2. Write a migrator that fills ONLY the new field from DB. DO NOT touch
--      'schemaVersion' inside the migrator — 'applyMigrations' stamps it.
--   3. Append 'MigrationEntry 2 backfillDriverTrustScore' below.
--   4. Do NOT remove existing entries — legacy v0 entries chain through them.
--
-- No constant to bump anywhere: 'currentSchemaVersion' moves with the list.
migrations :: [MigrationEntry]
migrations =
  [ MigrationEntry 1 backfillEffectiveBankAccount,
    MigrationEntry 2 backfillEnabled
  ]

-- | The "head" version, derived from the registry. Equals the largest
-- 'meVersion' in 'migrations', or 0 if the list is empty.
currentSchemaVersion :: Int
currentSchemaVersion = maximum (0 : map meVersion migrations)

-- | v1: introduce 'bankAccountPaymentMode' AND retroactively fix 'chargesEnabled'
-- to reflect the effective (fleet-or-driver) BA. Pre-migration entries had
-- 'chargesEnabled' populated from the driver's own BA only, which is wrong for
-- fleet drivers (their fleet owner holds the BA). One batched DB pass covers
-- both fields.
backfillEffectiveBankAccount ::
  (BeamFlow m r, MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Migrator m
backfillEffectiveBankAccount entries = do
  let personIds = map (cast . (.driverId)) entries :: [Id Person.Person]
  fleetAssocs <- QFDA.findAllByDriverIds personIds
  let faMap = HashMap.fromList $ map (\fa -> (cast fa.driverId :: Id Person.Person, fa)) fleetAssocs
      fleetOwnerPersonIds = DL.nub $ map (\fa -> Id @Person.Person fa.fleetOwnerId) fleetAssocs
  bankAccounts <- QDBA.getDriverBankAccounts (DL.nub (personIds <> fleetOwnerPersonIds))
  let baMap = HashMap.fromList $ map (\ba -> (ba.driverId, ba)) bankAccounts
  pure $
    map
      ( \e ->
          let pid = cast e.driverId :: Id Person.Person
              effective = case HashMap.lookup pid faMap of
                Just fa -> HashMap.lookup (Id @Person.Person fa.fleetOwnerId) baMap
                Nothing -> HashMap.lookup pid baMap
           in e
                { chargesEnabled = maybe False (.chargesEnabled) effective,
                  bankAccountPaymentMode = (.paymentMode) =<< effective
                  -- schemaVersion intentionally not set here — applyMigrations stamps it.
                }
      )
      entries

-- | v2: backfill the new 'enabled' flag on pool entries written before the
-- nearby-driver eligibility check started gating on it. Without this every
-- legacy entry would default to 'enabled = False' and be filtered out.
backfillEnabled ::
  (BeamFlow m r, MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Migrator m
backfillEnabled entries = do
  let driverIdTexts = map (getId . (.driverId)) entries
  dis <- QDI.findAllByDriverIds driverIdTexts
  let enabledMap = HashMap.fromList $ map (\di -> (cast di.driverId :: Id Person.Person, di.enabled)) dis
  pure $
    map
      (\e -> e {enabled = HashMap.lookupDefault e.enabled (cast e.driverId :: Id Person.Person) enabledMap})
      entries

-- | Walk the registry in ascending version order (sorted defensively in case
-- the source list isn't). For each step partition into 'due'
-- ('poolDataSchemaVersion < meVersion') and 'skip'; run the migrator on 'due'
-- only; stamp 'schemaVersion = Just meVersion' on each migrated result; merge
-- back. Returns each final entry paired with a flag indicating whether any
-- migrator touched it (so the caller persists only changed records).
applyMigrations ::
  (BeamFlow m r, MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  [DriverPoolData] ->
  m [(DriverPoolData, Bool)]
applyMigrations input = do
  let initial = map (,False) input
      ordered = sortOn meVersion migrations
  foldM step initial ordered
  where
    step working (MigrationEntry targetVersion migrator) = do
      let (due, skip) =
            partition
              (\(e, _) -> poolDataSchemaVersion e < targetVersion)
              working
      if null due
        then pure working
        else do
          migrated <- migrator (map fst due)
          let stamped = map (\e -> (e {schemaVersion = Just targetVersion}, True)) migrated
          pure $ skip <> stamped

    poolDataSchemaVersion :: DriverPoolData -> Int
    poolDataSchemaVersion = fromMaybe 0 . schemaVersion
