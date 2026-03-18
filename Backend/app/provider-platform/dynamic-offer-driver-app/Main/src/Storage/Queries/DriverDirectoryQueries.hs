module Storage.Queries.DriverDirectoryQueries where

import qualified Database.Beam as B
import Domain.Types.DriverInformation
import Domain.Types.FleetDriverAssociation
import Domain.Types.Person
import Domain.Types.VehicleCategory
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.DriverInformation as BeamDI
import qualified Storage.Beam.FleetDriverAssociation as BeamFDVA
import qualified Storage.Beam.Person as BeamP
import qualified Storage.Beam.Vehicle as BeamV
import Storage.Queries.OrphanInstances.DriverInformation ()
import Storage.Queries.OrphanInstances.Person ()

-- | Find all drivers eligible for fleet directory listing.
-- Eligible means: role = DRIVER, not blocked, enabled, in the given city,
-- and NOT already actively linked to the given fleet owner.
findAllEligibleDriversForDirectory ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  Text ->
  Maybe Text ->
  Maybe VehicleCategory ->
  Int ->
  Int ->
  m [(Person, DriverInformation)]
findAllEligibleDriversForDirectory fleetOwnerId cityId mbSearchTerm mbVehicleCategory limit offset = do
  now <- getCurrentTime
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (fromIntegral limit) $
            B.offset_ (fromIntegral offset) $
              B.orderBy_ (\(person, _driverInfo, _) -> B.desc_ person.createdAt) $
                B.filter_'
                  ( \(person, driverInfo, _vehicle) ->
                      person.role B.==?. B.val_ DRIVER
                        B.&&?. B.sqlBool_ (B.not_ driverInfo.blocked)
                        B.&&?. B.sqlBool_ driverInfo.enabled
                        B.&&?. person.merchantOperatingCityId B.==?. B.val_ (Just cityId)
                        B.&&?. maybe
                          (B.sqlBool_ $ B.val_ True)
                          (\searchTerm -> B.sqlBool_ (B.lower_ person.firstName `B.like_` B.lower_ (B.val_ ("%" <> searchTerm <> "%"))))
                          mbSearchTerm
                        B.&&?. maybe
                          (B.sqlBool_ $ B.val_ True)
                          (\vc -> _vehicle.category B.==?. B.val_ (Just vc))
                          mbVehicleCategory
                        -- Exclude drivers already actively linked to this fleet
                        B.&&?. B.sqlBool_
                          ( B.not_
                              ( B.exists_
                                  ( B.filter_
                                      ( \fda ->
                                          fda.driverId B.==. person.id
                                            B.&&. fda.fleetOwnerId B.==. B.val_ fleetOwnerId
                                            B.&&. fda.isActive B.==. B.val_ True
                                            B.&&. fda.associatedTill B.>=. B.val_ (Just now)
                                      )
                                      (B.all_ (BeamCommon.fleetDriverAssociation BeamCommon.atlasDB))
                                  )
                              )
                          )
                  )
                  do
                    person <- B.all_ (BeamCommon.person BeamCommon.atlasDB)
                    driverInfo <- B.join_ (BeamCommon.driverInformation BeamCommon.atlasDB) (\di -> BeamP.id person B.==. BeamDI.driverId di)
                    vehicle <- B.leftJoin_ (B.all_ (BeamCommon.vehicle BeamCommon.atlasDB)) (\v -> BeamP.id person B.==. BeamV.driverId v)
                    pure (person, driverInfo, vehicle)
  case res of
    Right rows -> do
      catMaybes <$> mapM (\(p, di, _v) -> do
        mbPerson <- fromTType' p
        mbDI <- fromTType' di
        pure $ liftA2 (,) mbPerson mbDI
        ) rows
    Left _ -> pure []

-- | Count all eligible drivers for pagination support.
countEligibleDriversForDirectory ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  Text ->
  Maybe Text ->
  Maybe VehicleCategory ->
  m Int
countEligibleDriversForDirectory fleetOwnerId cityId mbSearchTerm mbVehicleCategory = do
  now <- getCurrentTime
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
            B.filter_'
              ( \(person, driverInfo, _vehicle) ->
                  person.role B.==?. B.val_ DRIVER
                    B.&&?. B.sqlBool_ (B.not_ driverInfo.blocked)
                    B.&&?. B.sqlBool_ driverInfo.enabled
                    B.&&?. person.merchantOperatingCityId B.==?. B.val_ (Just cityId)
                    B.&&?. maybe
                      (B.sqlBool_ $ B.val_ True)
                      (\searchTerm -> B.sqlBool_ (B.lower_ person.firstName `B.like_` B.lower_ (B.val_ ("%" <> searchTerm <> "%"))))
                      mbSearchTerm
                    B.&&?. maybe
                      (B.sqlBool_ $ B.val_ True)
                      (\vc -> _vehicle.category B.==?. B.val_ (Just vc))
                      mbVehicleCategory
                    B.&&?. B.sqlBool_
                      ( B.not_
                          ( B.exists_
                              ( B.filter_
                                  ( \fda ->
                                      fda.driverId B.==. person.id
                                        B.&&. fda.fleetOwnerId B.==. B.val_ fleetOwnerId
                                        B.&&. fda.isActive B.==. B.val_ True
                                        B.&&. fda.associatedTill B.>=. B.val_ (Just now)
                                  )
                                  (B.all_ (BeamCommon.fleetDriverAssociation BeamCommon.atlasDB))
                              )
                          )
                      )
              )
              do
                person <- B.all_ (BeamCommon.person BeamCommon.atlasDB)
                driverInfo <- B.join_ (BeamCommon.driverInformation BeamCommon.atlasDB) (\di -> BeamP.id person B.==. BeamDI.driverId di)
                vehicle <- B.leftJoin_ (B.all_ (BeamCommon.vehicle BeamCommon.atlasDB)) (\v -> BeamP.id person B.==. BeamV.driverId v)
                pure (person, driverInfo, vehicle)
  case res of
    Right [count] -> pure count
    _ -> pure 0
