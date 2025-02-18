module Storage.Queries.FleetDriverAssociationExtra where

import qualified Database.Beam as B
import qualified Database.Beam.Query ()
import qualified Domain.Types.Common as DI
import Domain.Types.FleetDriverAssociation
import Domain.Types.Person
import Domain.Types.VehicleCategory
import Domain.Utils
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption (DbHash)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.DriverInformation as BeamDI
import qualified Storage.Beam.FleetDriverAssociation as BeamFDVA
import qualified Storage.Beam.Person as BeamP
import Storage.Queries.OrphanInstances.FleetDriverAssociation ()

-- Extra code goes here --

createFleetDriverAssociationIfNotExists :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Id Person -> VehicleCategory -> Bool -> m ()
createFleetDriverAssociationIfNotExists driverId fleetOwnerId onboardingVehicleCategory isActive = do
  now <- getCurrentTime
  mbFleetDriverAssociation <- findAllWithOptionsKV [Se.And [Se.Is BeamFDVA.driverId $ Se.Eq (driverId.getId), Se.Is BeamFDVA.fleetOwnerId $ Se.Eq fleetOwnerId.getId, Se.Is BeamFDVA.isActive $ Se.Eq isActive, Se.Is BeamFDVA.associatedTill (Se.GreaterThan $ Just now)]] (Se.Desc BeamFDVA.createdAt) (Just 1) Nothing <&> listToMaybe
  case mbFleetDriverAssociation of
    Just fleetDriverAssociation ->
      when (isNothing fleetDriverAssociation.onboardingVehicleCategory) $ do
        updateWithKV
          [ Se.Set BeamFDVA.onboardingVehicleCategory (Just onboardingVehicleCategory),
            Se.Set BeamFDVA.updatedAt now
          ]
          [Se.And [Se.Is BeamFDVA.id $ Se.Eq fleetDriverAssociation.id.getId]]
    Nothing -> do
      id <- generateGUID
      createWithKV $
        FleetDriverAssociation
          { associatedTill = convertTextToUTC (Just "2099-12-12"),
            driverId = driverId,
            fleetOwnerId = fleetOwnerId.getId,
            associatedOn = Just now,
            onboardingVehicleCategory = Just onboardingVehicleCategory,
            createdAt = now,
            updatedAt = now,
            ..
          }

findByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Id Person -> Bool -> m (Maybe FleetDriverAssociation))
findByDriverId driverId isActive = do
  now <- getCurrentTime
  findAllWithOptionsKV [Se.And [Se.Is BeamFDVA.driverId $ Se.Eq (driverId.getId), Se.Is BeamFDVA.isActive $ Se.Eq isActive, Se.Is BeamFDVA.associatedTill (Se.GreaterThan $ Just now)]] (Se.Desc BeamFDVA.createdAt) (Just 1) Nothing <&> listToMaybe

findByDriverIdAndFleetOwnerId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Id Person -> Text -> Bool -> m (Maybe FleetDriverAssociation))
findByDriverIdAndFleetOwnerId driverId fleetOwnerId isActive = do
  now <- getCurrentTime
  findAllWithOptionsKV [Se.And [Se.Is BeamFDVA.driverId $ Se.Eq (driverId.getId), Se.Is BeamFDVA.fleetOwnerId $ Se.Eq fleetOwnerId, Se.Is BeamFDVA.isActive $ Se.Eq isActive, Se.Is BeamFDVA.associatedTill (Se.GreaterThan $ Just now)]] (Se.Desc BeamFDVA.createdAt) (Just 1) Nothing <&> listToMaybe

findAllActiveDriverByFleetOwnerId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Text -> Int -> Int -> Maybe DbHash -> Maybe Text -> m [FleetDriverAssociation]
findAllActiveDriverByFleetOwnerId fleetOwnerId limit offset mbMobileNumberSearchStringHash mbName = do
  now <- getCurrentTime
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (toInteger limit) $
            B.offset_ (toInteger offset) $
              B.orderBy_ (\(fleetDriverAssociation', _) -> B.desc_ fleetDriverAssociation'.updatedAt) $
                B.filter_'
                  ( \(fleetDriverAssociation, driver) ->
                      fleetDriverAssociation.fleetOwnerId B.==?. B.val_ fleetOwnerId
                        B.&&?. fleetDriverAssociation.isActive B.==?. B.val_ True
                        B.&&?. B.sqlBool_ (fleetDriverAssociation.associatedTill B.>=. B.val_ (Just now))
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\mobileNumberSearchStringDB -> driver.mobileNumberHash B.==?. B.val_ (Just mobileNumberSearchStringDB)) mbMobileNumberSearchStringHash
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\name -> B.sqlBool_ (driver.firstName `B.like_` B.val_ ("%" <> name <> "%"))) mbName
                  )
                  do
                    fleetDriverAssociation <- B.all_ (BeamCommon.fleetDriverAssociation BeamCommon.atlasDB)
                    driver <- B.join_ (BeamCommon.person BeamCommon.atlasDB) (\driver -> BeamFDVA.driverId fleetDriverAssociation B.==. BeamP.id driver)
                    pure (fleetDriverAssociation, driver)
  case res of
    Right res' -> do
      let fleetDriverList = fst <$> res'
      catMaybes <$> mapM fromTType' fleetDriverList
    Left _ -> pure []

findAllDriverByFleetOwnerIdAndMbIsActive ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Text ->
  Maybe Bool ->
  Int ->
  Int ->
  m [FleetDriverAssociation]
findAllDriverByFleetOwnerIdAndMbIsActive fleetOwnerId mbIsActive limit offset = do
  now <- getCurrentTime
  findAllWithOptionsKV
    [ Se.And $
        [Se.Is BeamFDVA.fleetOwnerId $ Se.Eq fleetOwnerId, Se.Is BeamFDVA.associatedTill (Se.GreaterThan $ Just now)]
          <> [Se.Is BeamFDVA.isActive (Se.Eq isActive) | Just isActive <- [mbIsActive]]
    ]
    (Se.Desc BeamFDVA.updatedAt)
    (Just limit)
    (Just offset)

endFleetDriverAssociation :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Id Person -> m ()
endFleetDriverAssociation fleetOwnerId (Id driverId) = do
  now <- getCurrentTime
  updateWithKV
    [Se.Set BeamFDVA.associatedTill $ Just now, Se.Set BeamFDVA.isActive False]
    [Se.And [Se.Is BeamFDVA.fleetOwnerId (Se.Eq fleetOwnerId), Se.Is BeamFDVA.associatedTill (Se.GreaterThan $ Just now), Se.Is BeamFDVA.driverId (Se.Eq driverId)]]

findAllDriversByFleetOwnerIdByMode :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> DI.DriverMode -> Maybe Bool -> Integer -> Integer -> m [FleetDriverAssociation]
findAllDriversByFleetOwnerIdByMode fleetOwnerId mode mbIsActive limitVal offsetVal = do
  now <- getCurrentTime
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ limitVal $
            B.offset_ offsetVal $
              B.orderBy_ (\(rc', _) -> B.desc_ rc'.createdAt) $
                B.filter_'
                  ( \(fleetDriverAssociation, driverInformation) ->
                      fleetDriverAssociation.fleetOwnerId B.==?. B.val_ fleetOwnerId
                        B.&&?. driverInformation.mode B.==?. B.val_ (Just mode)
                        B.&&?. B.sqlBool_ (fleetDriverAssociation.associatedTill B.>=. B.val_ (Just now))
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\isActive -> fleetDriverAssociation.isActive B.==?. B.val_ isActive) mbIsActive
                  )
                  do
                    fleetDriverAssociation <- B.all_ (BeamCommon.fleetDriverAssociation BeamCommon.atlasDB)
                    driverInformation <- B.join_ (BeamCommon.driverInformation BeamCommon.atlasDB) (\driverInfo -> BeamFDVA.driverId fleetDriverAssociation B.==. BeamDI.driverId driverInfo)
                    pure (fleetDriverAssociation, driverInformation)
  case res of
    Right res' -> do
      let fleetDriverList = fst <$> res'
      catMaybes <$> mapM fromTType' fleetDriverList
    Left _ -> pure []
