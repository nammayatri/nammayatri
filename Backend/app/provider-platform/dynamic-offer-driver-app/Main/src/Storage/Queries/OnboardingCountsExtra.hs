-- | Postgres row counts for the onboarding buckets. These are the rebuild source when a Redis
--   counter key is cold: the ClickHouse mirrors carry only docsVerificationStatus, so they cannot
--   reproduce the flag-derived buckets.
--
--   Predicates here MUST match `bucketsOfFlags'` in
--   SharedLogic.DriverOnboarding.OnboardingFlags.Flow — that is the definition of a bucket.
module Storage.Queries.OnboardingCountsExtra
  ( OnboardingBucketCounts (..),
    countDriverBuckets,
    countFleetOwnerBuckets,
    countVehicleBuckets,
  )
where

import qualified Database.Beam as B
import qualified Database.Beam.Query ()
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Beam.Common as BeamCommon

data OnboardingBucketCounts = OnboardingBucketCounts
  { obcTotal :: Int,
    obcApproved :: Int,
    obcPending :: Int,
    obcRejected :: Int,
    obcEnabled :: Int,
    obcBlocked :: Int,
    obcDisabled :: Int
  }

-- Beam's SqlDB is parameterised over the backend, so the query is passed as-is to runDB at each
-- call site and only the result unwrapping is shared.
unwrapCount :: Either e (Maybe Int) -> Int
unwrapCount = \case
  Right (Just n) -> n
  _ -> 0

-- | Driver buckets for a city, optionally narrowed to one fleet via the driver's active
--   FleetDriverAssociation.
countDriverBuckets ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe Text ->
  m OnboardingBucketCounts
countDriverBuckets merchantOpCityId mbFleetOwnerId = do
  let cityMatches di = di.merchantOperatingCityId B.==. B.val_ (Just merchantOpCityId.getId)
      countWhere predicate =
        fmap unwrapCount $ do
          dbConf <- getReplicaBeamConfig
          L.runDB dbConf $
            L.findRow $
              B.select $
                B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
                  B.filter_
                    (\di -> cityMatches di B.&&. predicate di)
                    $ do
                      di <- B.all_ (BeamCommon.driverInformation BeamCommon.atlasDB)
                      case mbFleetOwnerId of
                        Nothing -> pure di
                        Just fleetOwnerId -> do
                          fda <-
                            B.join_
                              (BeamCommon.fleetDriverAssociation BeamCommon.atlasDB)
                              (\fda' -> fda'.driverId B.==. di.driverId)
                          B.guard_ (fda.fleetOwnerId B.==. B.val_ fleetOwnerId B.&&. fda.isActive B.==. B.val_ True)
                          pure di
  obcTotal <- countWhere (\_ -> B.val_ True)
  obcApproved <- countWhere (\di -> di.verified B.==. B.val_ True B.&&. di.approved B.==. B.val_ (Just True))
  obcPending <- countWhere (\di -> B.isNothing_ di.approved)
  obcRejected <- countWhere (\di -> di.verified B.==. B.val_ False B.&&. di.approved B.==. B.val_ (Just False))
  obcEnabled <- countWhere (\di -> di.verified B.==. B.val_ True B.&&. di.approved B.==. B.val_ (Just True) B.&&. di.enabled B.==. B.val_ True)
  obcBlocked <- countWhere (\di -> di.blocked B.==. B.val_ True)
  obcDisabled <- countWhere (\di -> B.not_ (B.isNothing_ di.disabledReasonFlag))
  pure OnboardingBucketCounts {..}

countFleetOwnerBuckets ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe Text ->
  m OnboardingBucketCounts
countFleetOwnerBuckets merchantOpCityId mbFleetOwnerId = do
  let countWhere predicate =
        fmap unwrapCount $ do
          dbConf <- getReplicaBeamConfig
          L.runDB dbConf $
            L.findRow $
              B.select $
                B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
                  B.filter_
                    ( \foi ->
                        foi.merchantOperatingCityId B.==. B.val_ (Just merchantOpCityId.getId)
                          B.&&. maybe (B.val_ True) (\fleetOwnerId -> foi.fleetOwnerPersonId B.==. B.val_ fleetOwnerId) mbFleetOwnerId
                          B.&&. predicate foi
                    )
                    (B.all_ (BeamCommon.fleetOwnerInformation BeamCommon.atlasDB))
  obcTotal <- countWhere (\_ -> B.val_ True)
  obcApproved <- countWhere (\foi -> foi.verified B.==. B.val_ True B.&&. foi.approved B.==. B.val_ (Just True))
  obcPending <- countWhere (\foi -> B.isNothing_ foi.approved)
  obcRejected <- countWhere (\foi -> foi.verified B.==. B.val_ False B.&&. foi.approved B.==. B.val_ (Just False))
  obcEnabled <- countWhere (\foi -> foi.verified B.==. B.val_ True B.&&. foi.approved B.==. B.val_ (Just True) B.&&. foi.enabled B.==. B.val_ True)
  obcBlocked <- countWhere (\foi -> foi.blocked B.==. B.val_ True)
  obcDisabled <- countWhere (\foi -> B.not_ (B.isNothing_ foi.disabledReasonFlag))
  pure OnboardingBucketCounts {..}

-- | Vehicles have no enabled / blocked / disabled flags, so those buckets are always zero — the
--   same values the recompute writes for them.
countVehicleBuckets ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe Text ->
  m OnboardingBucketCounts
countVehicleBuckets merchantOpCityId mbFleetOwnerId = do
  let countWhere predicate =
        fmap unwrapCount $ do
          dbConf <- getReplicaBeamConfig
          L.runDB dbConf $
            L.findRow $
              B.select $
                B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
                  B.filter_
                    ( \rc ->
                        rc.merchantOperatingCityId B.==. B.val_ (Just merchantOpCityId.getId)
                          B.&&. maybe (B.val_ True) (\fleetOwnerId -> rc.fleetOwnerId B.==. B.val_ (Just fleetOwnerId)) mbFleetOwnerId
                          B.&&. predicate rc
                    )
                    (B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB))
  obcTotal <- countWhere (\_ -> B.val_ True)
  obcApproved <- countWhere (\rc -> rc.verified B.==. B.val_ (Just True) B.&&. rc.approved B.==. B.val_ (Just True))
  obcPending <- countWhere (\rc -> B.isNothing_ rc.approved)
  obcRejected <- countWhere (\rc -> rc.verified B.==. B.val_ (Just False) B.&&. rc.approved B.==. B.val_ (Just False))
  pure OnboardingBucketCounts {obcEnabled = 0, obcBlocked = 0, obcDisabled = 0, ..}
