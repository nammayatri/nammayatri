-- | Backing queries for the city-wide vehicle onboarding grid.
module Storage.Queries.OnboardingList.VehicleList
  ( findVehicles,
  )
where

import qualified Database.Beam as B
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.VehicleRegistrationCertificate
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption (DbHash)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Beam.Common as BeamCommon
import Storage.Queries.OrphanInstances.VehicleRegistrationCertificate ()

findVehicles ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Int ->
  Int ->
  Maybe Text ->
  Maybe DbHash ->
  Maybe Bool ->
  Maybe (Maybe Bool) ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  m [VehicleRegistrationCertificate]
findVehicles merchantId merchantOpCityId limitVal offsetVal mbFleetOwnerId mbCertificateNumberHash mbVerified mbApprovalFilter mbFrom mbTo = do
  dbConf <- getReplicaBeamConfig
  result <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (fromIntegral limitVal) $
            B.offset_ (fromIntegral offsetVal) $
              B.orderBy_ (\rc -> (B.desc_ rc.createdAt, B.asc_ rc.id)) $
                B.filter_
                  ( \rc ->
                      rc.merchantId B.==. B.val_ (Just merchantId.getId)
                        B.&&. rc.merchantOperatingCityId B.==. B.val_ (Just merchantOpCityId.getId)
                        B.&&. maybe (B.val_ True) (\fleetOwnerId -> rc.fleetOwnerId B.==. B.val_ (Just fleetOwnerId)) mbFleetOwnerId
                        B.&&. maybe (B.val_ True) (\certHash -> rc.certificateNumberHash B.==. B.val_ certHash) mbCertificateNumberHash
                        B.&&. maybe (B.val_ True) (\verified -> rc.verified B.==. B.val_ (Just verified)) mbVerified
                        B.&&. ( case mbApprovalFilter of
                                  Nothing -> B.val_ True
                                  Just Nothing -> B.isNothing_ rc.approved B.&&. rc.verified B.==. B.val_ (Just True)
                                  Just (Just approved) -> rc.approved B.==. B.val_ (Just approved)
                              )
                        B.&&. maybe (B.val_ True) (\fromTime -> rc.createdAt B.>=. B.val_ fromTime) mbFrom
                        B.&&. maybe (B.val_ True) (\toTime -> rc.createdAt B.<=. B.val_ toTime) mbTo
                  )
                  (B.all_ (BeamCommon.vehicleRegistrationCertificate BeamCommon.atlasDB))
  case result of
    Right rcs -> catMaybes <$> mapM fromTType' rcs
    Left _ -> pure []
