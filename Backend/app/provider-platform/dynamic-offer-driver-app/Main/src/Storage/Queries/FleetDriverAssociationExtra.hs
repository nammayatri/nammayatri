module Storage.Queries.FleetDriverAssociationExtra where

import Control.Applicative (liftA2, liftA3)
import Data.Text (takeEnd, toLower)
import qualified Database.Beam as B
import qualified Database.Beam.Query ()
import qualified Domain.Types.Common as DI
import Domain.Types.DriverInformation
import Domain.Types.FleetDriverAssociation
import Domain.Types.Person
import Domain.Types.VehicleCategory
import Domain.Utils
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption (DbHash, getDbHash)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.DriverInformation as BeamDI
import qualified Storage.Beam.FleetDriverAssociation as BeamFDVA
import qualified Storage.Beam.Person as BeamP
import qualified Storage.Beam.Vehicle as BeamV
import Storage.Queries.OrphanInstances.DriverInformation ()
import Storage.Queries.OrphanInstances.FleetDriverAssociation ()
import Storage.Queries.OrphanInstances.Person ()

-- Extra code goes here --

createFleetDriverAssociationIfNotExists ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Person ->
  Id Person ->
  Maybe (Id Person) ->
  VehicleCategory ->
  Bool ->
  Maybe Text ->
  m ()
createFleetDriverAssociationIfNotExists driverId fleetOwnerId onboardedOperatorId onboardingVehicleCategory isActive requestReason = do
  now <- getCurrentTime
  Redis.withWaitOnLockRedisWithExpiry (driverFleetLockKey driverId.getId fleetOwnerId.getId) 10 10 $ do
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
              onboardedOperatorId,
              createdAt = now,
              updatedAt = now,
              responseReason = Nothing,
              ..
            }
  where
    driverFleetLockKey :: Text -> Text -> Text
    driverFleetLockKey dId fId = "fleet_driver_association:driver:" <> dId <> ":fleet_owner:" <> fId

findByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Id Person -> Bool -> m (Maybe FleetDriverAssociation))
findByDriverId driverId isActive = do
  now <- getCurrentTime
  findAllWithOptionsKV [Se.And [Se.Is BeamFDVA.driverId $ Se.Eq (driverId.getId), Se.Is BeamFDVA.isActive $ Se.Eq isActive, Se.Is BeamFDVA.associatedTill (Se.GreaterThan $ Just now)]] (Se.Desc BeamFDVA.createdAt) (Just 1) Nothing <&> listToMaybe

findAllByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id Person ->
  Bool ->
  m [FleetDriverAssociation]
findAllByDriverId driverId isActive = do
  now <- getCurrentTime
  findAllWithOptionsKV [Se.And [Se.Is BeamFDVA.driverId $ Se.Eq (driverId.getId), Se.Is BeamFDVA.isActive $ Se.Eq isActive, Se.Is BeamFDVA.associatedTill (Se.GreaterThan $ Just now)]] (Se.Desc BeamFDVA.createdAt) Nothing Nothing

findAllByDriverIdWithStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id Person ->
  m [FleetDriverAssociation]
findAllByDriverIdWithStatus driverId = do
  now <- getCurrentTime
  findAllWithOptionsKV [Se.And [Se.Is BeamFDVA.driverId $ Se.Eq (driverId.getId), Se.Is BeamFDVA.associatedTill (Se.GreaterThan $ Just now)]] (Se.Desc BeamFDVA.createdAt) Nothing Nothing

findAllByDriverIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Id Person] ->
  m [FleetDriverAssociation]
findAllByDriverIds driverIds = do
  now <- getCurrentTime
  findAllWithKV [Se.And [Se.Is BeamFDVA.driverId $ Se.In (getId <$> driverIds), Se.Is BeamFDVA.isActive $ Se.Eq True, Se.Is BeamFDVA.associatedTill (Se.GreaterThan $ Just now)]]

getActiveDriverIdsByFleetOwnerId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Text ->
  m [Id Person]
getActiveDriverIdsByFleetOwnerId fleetOwnerId = do
  now <- getCurrentTime
  associations <-
    findAllWithKV
      [ Se.And
          [ Se.Is BeamFDVA.fleetOwnerId $ Se.Eq fleetOwnerId,
            Se.Is BeamFDVA.isActive $ Se.Eq True,
            Se.Is BeamFDVA.associatedTill (Se.GreaterThan $ Just now)
          ]
      ]
  pure $ map (.driverId) associations

findByDriverIdAndFleetOwnerId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Id Person -> Text -> Bool -> m (Maybe FleetDriverAssociation))
findByDriverIdAndFleetOwnerId driverId fleetOwnerId isActive = do
  now <- getCurrentTime
  findAllWithOptionsKV [Se.And [Se.Is BeamFDVA.driverId $ Se.Eq (driverId.getId), Se.Is BeamFDVA.fleetOwnerId $ Se.Eq fleetOwnerId, Se.Is BeamFDVA.isActive $ Se.Eq isActive, Se.Is BeamFDVA.associatedTill (Se.GreaterThan $ Just now)]] (Se.Desc BeamFDVA.createdAt) (Just 1) Nothing <&> listToMaybe

findAllActiveDriverByFleetOwnerIdWithDriverInfo :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r, EncFlow m r) => Text -> Int -> Int -> Maybe DbHash -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe DI.DriverMode -> m [(FleetDriverAssociation, Person, DriverInformation)]
findAllActiveDriverByFleetOwnerIdWithDriverInfo fleetOwnerId limit offset mbMobileNumberSearchStringHash mbName mbSearchString mbIsActive mbMode = do
  now <- getCurrentTime
  dbConf <- getReplicaBeamConfig
  encryptedMobileNumberHash <- mapM getDbHash mbSearchString

  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (toInteger limit) $
            B.offset_ (toInteger offset) $
              B.orderBy_ (\(fleetDriverAssociation', _, _) -> B.desc_ fleetDriverAssociation'.updatedAt) $
                B.filter_'
                  ( \(fleetDriverAssociation, driver, driverInformation) ->
                      fleetDriverAssociation.fleetOwnerId B.==?. B.val_ fleetOwnerId
                        B.&&?. B.sqlBool_ (fleetDriverAssociation.associatedTill B.>=. B.val_ (Just now))
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\mode -> driverInformation.mode B.==?. B.val_ (Just mode)) mbMode
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\isActive -> fleetDriverAssociation.isActive B.==?. B.val_ isActive) mbIsActive
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\name -> B.sqlBool_ (B.lower_ driver.firstName `B.like_` B.lower_ (B.val_ ("%" <> name <> "%")))) mbName
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\mobileNumberSearchStringDB -> driver.mobileNumberHash B.==?. B.val_ (Just mobileNumberSearchStringDB)) mbMobileNumberSearchStringHash
                        B.&&?. ( maybe (B.sqlBool_ $ B.val_ True) (\name -> B.sqlBool_ (B.lower_ driver.firstName `B.like_` (B.val_ ("%" <> toLower name <> "%")))) mbSearchString
                                   B.||?. maybe (B.sqlBool_ $ B.val_ True) (\searchString -> B.sqlBool_ (B.like_ (B.coalesce_ [driver.maskedMobileDigits] (B.val_ "")) (B.val_ ("%" <> searchString <> "%")))) mbSearchString
                                   B.||?. maybe (B.sqlBool_ $ B.val_ True) (\mobileNumberSearchStringDB -> driver.mobileNumberHash B.==?. B.val_ (Just mobileNumberSearchStringDB)) encryptedMobileNumberHash
                               )
                  )
                  do
                    fleetDriverAssociation <- B.all_ (BeamCommon.fleetDriverAssociation BeamCommon.atlasDB)
                    driver <- B.join_ (BeamCommon.person BeamCommon.atlasDB) (\driver -> BeamFDVA.driverId fleetDriverAssociation B.==. BeamP.id driver)
                    driverInformation <- B.join_ (BeamCommon.driverInformation BeamCommon.atlasDB) (\driverInfo -> BeamP.id driver B.==. BeamDI.driverId driverInfo)
                    pure (fleetDriverAssociation, driver, driverInformation)
  case res of
    Right res' -> do
      let fleetDriverList = (\(fleetDriverAssociation, driver, driverInformation) -> (fleetDriverAssociation, driver, driverInformation)) <$> res'
      catMaybes <$> mapM (\(f, d, di) -> liftA3 (,,) <$> fromTType' f <*> fromTType' d <*> fromTType' di) fleetDriverList
    Left _ -> pure []

findAllActiveDriverByFleetOwnerId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r, EncFlow m r) => Text -> Maybe Int -> Maybe Int -> Maybe DbHash -> Maybe Text -> Maybe Text -> Maybe Bool -> m [(FleetDriverAssociation, Person)]
findAllActiveDriverByFleetOwnerId fleetOwnerId Nothing Nothing mbMobileNumberSearchStringHash mbName mbSearchString mbIsActive = do
  now <- getCurrentTime
  dbConf <- getReplicaBeamConfig
  encryptedMobileNumberHash <- mapM getDbHash mbSearchString
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.orderBy_ (\(fleetDriverAssociation', _, _) -> B.desc_ fleetDriverAssociation'.updatedAt) $
            B.filter_'
              ( \(fleetDriverAssociation, driver, _) ->
                  fleetDriverAssociation.fleetOwnerId B.==?. B.val_ fleetOwnerId
                    B.&&?. B.sqlBool_ (fleetDriverAssociation.associatedTill B.>=. B.val_ (Just now))
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\isActive -> fleetDriverAssociation.isActive B.==?. B.val_ isActive) mbIsActive
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\name -> B.sqlBool_ (B.lower_ driver.firstName `B.like_` B.lower_ (B.val_ ("%" <> name <> "%")))) mbName
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\mobileNumberSearchStringDB -> driver.mobileNumberHash B.==?. B.val_ (Just mobileNumberSearchStringDB)) mbMobileNumberSearchStringHash
                    B.&&?. ( maybe (B.sqlBool_ $ B.val_ True) (\name -> B.sqlBool_ (B.lower_ driver.firstName `B.like_` B.lower_ (B.val_ ("%" <> name <> "%")))) mbSearchString
                               B.||?. maybe (B.sqlBool_ $ B.val_ True) (\searchString -> B.sqlBool_ (B.like_ (B.coalesce_ [driver.maskedMobileDigits] (B.val_ "")) (B.val_ ("%" <> searchString <> "%")))) mbSearchString
                               B.||?. maybe (B.sqlBool_ $ B.val_ True) (\mobileNumberSearchStringDB -> driver.mobileNumberHash B.==?. B.val_ (Just mobileNumberSearchStringDB)) encryptedMobileNumberHash
                           )
              )
              do
                fleetDriverAssociation <- B.all_ (BeamCommon.fleetDriverAssociation BeamCommon.atlasDB)
                driver <- B.join_ (BeamCommon.person BeamCommon.atlasDB) (\driver -> BeamFDVA.driverId fleetDriverAssociation B.==. BeamP.id driver)
                vehicle <- B.join_ (BeamCommon.vehicle BeamCommon.atlasDB) (\vehicle -> BeamFDVA.driverId fleetDriverAssociation B.==. BeamV.driverId vehicle)
                pure (fleetDriverAssociation, driver, vehicle)
  case res of
    Right fleetDriverList ->
      catMaybes <$> mapM (\(f, d, _) -> liftA2 (,) <$> fromTType' f <*> fromTType' d) fleetDriverList
    Left _ -> pure []
findAllActiveDriverByFleetOwnerId fleetOwnerId mbLimit mbOffset mbMobileNumberSearchStringHash mbName mbSearchString mbIsActive = do
  now <- getCurrentTime
  dbConf <- getReplicaBeamConfig
  encryptedMobileNumberHash <- mapM getDbHash mbSearchString
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (fromIntegral limit) $
            B.offset_ (fromIntegral offset) $
              B.orderBy_ (\(fleetDriverAssociation', _, _) -> B.desc_ fleetDriverAssociation'.updatedAt) $
                B.filter_'
                  ( \(fleetDriverAssociation, driver, _) ->
                      fleetDriverAssociation.fleetOwnerId B.==?. B.val_ fleetOwnerId
                        B.&&?. B.sqlBool_ (fleetDriverAssociation.associatedTill B.>=. B.val_ (Just now))
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\isActive -> fleetDriverAssociation.isActive B.==?. B.val_ isActive) mbIsActive
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\name -> B.sqlBool_ (B.lower_ driver.firstName `B.like_` B.lower_ (B.val_ ("%" <> name <> "%")))) mbName
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\mobileNumberSearchStringDB -> driver.mobileNumberHash B.==?. B.val_ (Just mobileNumberSearchStringDB)) mbMobileNumberSearchStringHash
                        B.&&?. ( maybe (B.sqlBool_ $ B.val_ True) (\name -> B.sqlBool_ (B.lower_ driver.firstName `B.like_` B.lower_ (B.val_ ("%" <> name <> "%")))) mbSearchString
                                   B.||?. maybe (B.sqlBool_ $ B.val_ True) (\searchString -> B.sqlBool_ (B.like_ (B.coalesce_ [driver.maskedMobileDigits] (B.val_ "")) (B.val_ ("%" <> searchString <> "%")))) mbSearchString
                                   B.||?. maybe (B.sqlBool_ $ B.val_ True) (\mobileNumberSearchStringDB -> driver.mobileNumberHash B.==?. B.val_ (Just mobileNumberSearchStringDB)) encryptedMobileNumberHash
                               )
                  )
                  do
                    fleetDriverAssociation <- B.all_ (BeamCommon.fleetDriverAssociation BeamCommon.atlasDB)
                    driver <- B.join_ (BeamCommon.person BeamCommon.atlasDB) (\driver -> BeamFDVA.driverId fleetDriverAssociation B.==. BeamP.id driver)
                    vehicle <- B.join_ (BeamCommon.vehicle BeamCommon.atlasDB) (\vehicle -> BeamFDVA.driverId fleetDriverAssociation B.==. BeamV.driverId vehicle)
                    pure (fleetDriverAssociation, driver, vehicle)
  case res of
    Right fleetDriverList ->
      catMaybes <$> mapM (\(f, d, _) -> liftA2 (,) <$> fromTType' f <*> fromTType' d) fleetDriverList
    Left _ -> pure []

findAllInactiveDriverByFleetOwnerId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r, EncFlow m r) => Text -> Maybe Int -> Maybe Int -> Maybe DbHash -> Maybe Text -> Maybe Text -> m [(FleetDriverAssociation, Person)]
findAllInactiveDriverByFleetOwnerId fleetOwnerId mbLimit mbOffset mbMobileNumberSearchStringHash mbName mbSearchString = do
  dbConf <- getReplicaBeamConfig
  encryptedMobileNumberHash <- mapM getDbHash mbSearchString
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  allActiveDriverIds <- findAllActiveDriverByFleetOwnerId fleetOwnerId Nothing Nothing mbMobileNumberSearchStringHash mbName mbSearchString (Just True)
  let allActiveDriverIds' = (\(_, driver) -> driver.id) <$> allActiveDriverIds
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (fromIntegral limit) $
            B.offset_ (fromIntegral offset) $
              B.orderBy_ (\(fleetDriverAssociation', _) -> B.desc_ fleetDriverAssociation'.updatedAt) $
                B.filter_'
                  ( \(fleetDriverAssociation, driver) ->
                      fleetDriverAssociation.fleetOwnerId B.==?. B.val_ fleetOwnerId
                        B.&&?. B.sqlBool_ (B.not_ (fleetDriverAssociation.driverId `B.in_` (B.val_ . getId <$> allActiveDriverIds')))
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\name -> B.sqlBool_ (B.lower_ driver.firstName `B.like_` B.lower_ (B.val_ ("%" <> name <> "%")))) mbName
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\mobileNumberSearchStringDB -> driver.mobileNumberHash B.==?. B.val_ (Just mobileNumberSearchStringDB)) mbMobileNumberSearchStringHash
                        B.&&?. ( maybe (B.sqlBool_ $ B.val_ True) (\name -> B.sqlBool_ (B.lower_ driver.firstName `B.like_` B.lower_ (B.val_ ("%" <> name <> "%")))) mbSearchString
                                   B.||?. maybe (B.sqlBool_ $ B.val_ True) (\lastDigits -> B.sqlBool_ (B.like_ (B.coalesce_ [driver.maskedMobileDigits] (B.val_ "")) (B.val_ ("%" <> takeEnd 4 lastDigits <> "%")))) mbSearchString
                                   B.||?. maybe (B.sqlBool_ $ B.val_ True) (\mobileNumberSearchStringDB -> driver.mobileNumberHash B.==?. B.val_ (Just mobileNumberSearchStringDB)) encryptedMobileNumberHash
                               )
                  )
                  do
                    fleetDriverAssociation <- B.all_ (BeamCommon.fleetDriverAssociation BeamCommon.atlasDB)
                    driver <- B.join_ (BeamCommon.person BeamCommon.atlasDB) (\driver -> BeamFDVA.driverId fleetDriverAssociation B.==. BeamP.id driver)
                    pure (fleetDriverAssociation, driver)
  case res of
    Right fleetDriverList -> catMaybes <$> mapM (\(f, d) -> liftA2 (,) <$> fromTType' f <*> fromTType' d) fleetDriverList
    Left _ -> pure []

findAllDriverByFleetOwnerId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r, EncFlow m r) => Text -> Maybe Int -> Maybe Int -> Maybe DbHash -> Maybe Text -> Maybe Text -> m [(FleetDriverAssociation, Person)]
findAllDriverByFleetOwnerId fleetOwnerId mbLimit mbOffset mbMobileNumberSearchStringHash mbName mbSearchString = do
  dbConf <- getReplicaBeamConfig
  now <- getCurrentTime
  encryptedMobileNumberHash <- mapM getDbHash mbSearchString
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (fromIntegral limit) $
            B.offset_ (fromIntegral offset) $
              B.orderBy_ (\(fleetDriverAssociation', _) -> B.desc_ fleetDriverAssociation'.updatedAt) $
                B.filter_'
                  ( \(fleetDriverAssociation, driver) ->
                      fleetDriverAssociation.fleetOwnerId B.==?. B.val_ fleetOwnerId
                        B.&&?. B.sqlBool_ (fleetDriverAssociation.associatedTill B.>=. B.val_ (Just now))
                        B.&&?. (fleetDriverAssociation.isActive B.==?. B.val_ True)
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\name -> B.sqlBool_ (B.lower_ driver.firstName `B.like_` B.lower_ (B.val_ ("%" <> name <> "%")))) mbName
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\mobileNumberSearchStringDB -> driver.mobileNumberHash B.==?. B.val_ (Just mobileNumberSearchStringDB)) mbMobileNumberSearchStringHash
                        B.&&?. ( maybe (B.sqlBool_ $ B.val_ True) (\name -> B.sqlBool_ (B.lower_ driver.firstName `B.like_` B.lower_ (B.val_ ("%" <> name <> "%")))) mbSearchString
                                   B.||?. maybe (B.sqlBool_ $ B.val_ True) (\searchString -> B.sqlBool_ (B.like_ (B.coalesce_ [driver.maskedMobileDigits] (B.val_ "")) (B.val_ ("%" <> searchString <> "%")))) mbSearchString
                                   B.||?. maybe (B.sqlBool_ $ B.val_ True) (\mobileNumberSearchStringDB -> driver.mobileNumberHash B.==?. B.val_ (Just mobileNumberSearchStringDB)) encryptedMobileNumberHash
                               )
                  )
                  do
                    fleetDriverAssociation <- B.all_ (BeamCommon.fleetDriverAssociation BeamCommon.atlasDB)
                    driver <- B.join_ (BeamCommon.person BeamCommon.atlasDB) (\driver -> BeamFDVA.driverId fleetDriverAssociation B.==. BeamP.id driver)
                    pure (fleetDriverAssociation, driver)
  case res of
    Right fleetDriverList -> catMaybes <$> mapM (\(f, d) -> liftA2 (,) <$> fromTType' f <*> fromTType' d) fleetDriverList
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

findActiveDriverByFleetOwnerId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Text ->
  m (Maybe FleetDriverAssociation)
findActiveDriverByFleetOwnerId fleetOwnerId = do
  now <- getCurrentTime
  listToMaybe
    <$> findAllWithOptionsKV'
      [ Se.And
          [ Se.Is BeamFDVA.fleetOwnerId $ Se.Eq fleetOwnerId,
            Se.Is BeamFDVA.isActive $ Se.Eq True,
            Se.Is BeamFDVA.associatedTill (Se.GreaterThan $ Just now)
          ]
      ]
      (Just 1)
      Nothing

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

----------------------------- multi fleet owner queries ----------------------------------

findAllActiveDriverByFleetOwnerIds :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r, EncFlow m r) => [Text] -> Maybe Int -> Maybe Int -> Maybe DbHash -> Maybe Text -> Maybe Text -> Maybe Bool -> m [(FleetDriverAssociation, Person)]
findAllActiveDriverByFleetOwnerIds fleetOwnerIds Nothing Nothing mbMobileNumberSearchStringHash mbName mbSearchString mbIsActive = do
  now <- getCurrentTime
  dbConf <- getReplicaBeamConfig
  encryptedMobileNumberHash <- mapM getDbHash mbSearchString
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.orderBy_ (\(fleetDriverAssociation', _, _) -> B.desc_ fleetDriverAssociation'.updatedAt) $
            B.filter_'
              ( \(fleetDriverAssociation, driver, _) ->
                  (B.sqlBool_ (fleetDriverAssociation.fleetOwnerId `B.in_` (B.val_ <$> fleetOwnerIds)))
                    B.&&?. B.sqlBool_ (fleetDriverAssociation.associatedTill B.>=. B.val_ (Just now))
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\isActive -> fleetDriverAssociation.isActive B.==?. B.val_ isActive) mbIsActive
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\name -> B.sqlBool_ (B.lower_ driver.firstName `B.like_` B.lower_ (B.val_ ("%" <> name <> "%")))) mbName
                    B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\mobileNumberSearchStringDB -> driver.mobileNumberHash B.==?. B.val_ (Just mobileNumberSearchStringDB)) mbMobileNumberSearchStringHash
                    B.&&?. ( maybe (B.sqlBool_ $ B.val_ True) (\name -> B.sqlBool_ (B.lower_ driver.firstName `B.like_` B.lower_ (B.val_ ("%" <> name <> "%")))) mbSearchString
                               B.||?. maybe (B.sqlBool_ $ B.val_ True) (\lastDigits -> B.sqlBool_ (B.like_ (B.coalesce_ [driver.maskedMobileDigits] (B.val_ "")) (B.val_ ("%" <> takeEnd 4 lastDigits <> "%")))) mbSearchString
                               B.||?. maybe (B.sqlBool_ $ B.val_ True) (\mobileNumberSearchStringDB -> driver.mobileNumberHash B.==?. B.val_ (Just mobileNumberSearchStringDB)) encryptedMobileNumberHash
                           )
              )
              do
                fleetDriverAssociation <- B.all_ (BeamCommon.fleetDriverAssociation BeamCommon.atlasDB)
                driver <- B.join_ (BeamCommon.person BeamCommon.atlasDB) (\driver -> BeamFDVA.driverId fleetDriverAssociation B.==. BeamP.id driver)
                vehicle <- B.join_ (BeamCommon.vehicle BeamCommon.atlasDB) (\vehicle -> BeamFDVA.driverId fleetDriverAssociation B.==. BeamV.driverId vehicle)
                pure (fleetDriverAssociation, driver, vehicle)
  case res of
    Right fleetDriverList ->
      catMaybes <$> mapM (\(f, d, _) -> liftA2 (,) <$> fromTType' f <*> fromTType' d) fleetDriverList
    Left _ -> pure []
findAllActiveDriverByFleetOwnerIds fleetOwnerIds mbLimit mbOffset mbMobileNumberSearchStringHash mbName mbSearchString mbIsActive = do
  now <- getCurrentTime
  dbConf <- getReplicaBeamConfig
  encryptedMobileNumberHash <- mapM getDbHash mbSearchString
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (fromIntegral limit) $
            B.offset_ (fromIntegral offset) $
              B.orderBy_ (\(fleetDriverAssociation', _, _) -> B.desc_ fleetDriverAssociation'.updatedAt) $
                B.filter_'
                  ( \(fleetDriverAssociation, driver, _) ->
                      (B.sqlBool_ (fleetDriverAssociation.fleetOwnerId `B.in_` (B.val_ <$> fleetOwnerIds)))
                        B.&&?. B.sqlBool_ (fleetDriverAssociation.associatedTill B.>=. B.val_ (Just now))
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\isActive -> fleetDriverAssociation.isActive B.==?. B.val_ isActive) mbIsActive
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\name -> B.sqlBool_ (B.lower_ driver.firstName `B.like_` B.lower_ (B.val_ ("%" <> name <> "%")))) mbName
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\mobileNumberSearchStringDB -> driver.mobileNumberHash B.==?. B.val_ (Just mobileNumberSearchStringDB)) mbMobileNumberSearchStringHash
                        B.&&?. ( maybe (B.sqlBool_ $ B.val_ True) (\name -> B.sqlBool_ (B.lower_ driver.firstName `B.like_` B.lower_ (B.val_ ("%" <> name <> "%")))) mbSearchString
                                   B.||?. maybe (B.sqlBool_ $ B.val_ True) (\lastDigits -> B.sqlBool_ (B.like_ (B.coalesce_ [driver.maskedMobileDigits] (B.val_ "")) (B.val_ ("%" <> takeEnd 4 lastDigits <> "%")))) mbSearchString
                                   B.||?. maybe (B.sqlBool_ $ B.val_ True) (\mobileNumberSearchStringDB -> driver.mobileNumberHash B.==?. B.val_ (Just mobileNumberSearchStringDB)) encryptedMobileNumberHash
                               )
                  )
                  do
                    fleetDriverAssociation <- B.all_ (BeamCommon.fleetDriverAssociation BeamCommon.atlasDB)
                    driver <- B.join_ (BeamCommon.person BeamCommon.atlasDB) (\driver -> BeamFDVA.driverId fleetDriverAssociation B.==. BeamP.id driver)
                    vehicle <- B.join_ (BeamCommon.vehicle BeamCommon.atlasDB) (\vehicle -> BeamFDVA.driverId fleetDriverAssociation B.==. BeamV.driverId vehicle)
                    pure (fleetDriverAssociation, driver, vehicle)
  case res of
    Right fleetDriverList ->
      catMaybes <$> mapM (\(f, d, _) -> liftA2 (,) <$> fromTType' f <*> fromTType' d) fleetDriverList
    Left _ -> pure []

findAllInactiveDriverByFleetOwnerIds :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r, EncFlow m r) => [Text] -> Maybe Int -> Maybe Int -> Maybe DbHash -> Maybe Text -> Maybe Text -> m [(FleetDriverAssociation, Person)]
findAllInactiveDriverByFleetOwnerIds fleetOwnerIds mbLimit mbOffset mbMobileNumberSearchStringHash mbName mbSearchString = do
  dbConf <- getReplicaBeamConfig
  encryptedMobileNumberHash <- mapM getDbHash mbSearchString
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  allActiveDriverIds <- findAllActiveDriverByFleetOwnerIds fleetOwnerIds Nothing Nothing mbMobileNumberSearchStringHash mbName mbSearchString (Just True)
  let allActiveDriverIds' = (\(_, driver) -> driver.id) <$> allActiveDriverIds
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (fromIntegral limit) $
            B.offset_ (fromIntegral offset) $
              B.orderBy_ (\(fleetDriverAssociation', _) -> B.desc_ fleetDriverAssociation'.updatedAt) $
                B.filter_'
                  ( \(fleetDriverAssociation, driver) ->
                      (B.sqlBool_ (fleetDriverAssociation.fleetOwnerId `B.in_` (B.val_ <$> fleetOwnerIds)))
                        B.&&?. B.sqlBool_ (B.not_ (fleetDriverAssociation.driverId `B.in_` (B.val_ . getId <$> allActiveDriverIds')))
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\name -> B.sqlBool_ (B.lower_ driver.firstName `B.like_` B.lower_ (B.val_ ("%" <> name <> "%")))) mbName
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\mobileNumberSearchStringDB -> driver.mobileNumberHash B.==?. B.val_ (Just mobileNumberSearchStringDB)) mbMobileNumberSearchStringHash
                        B.&&?. ( maybe (B.sqlBool_ $ B.val_ True) (\name -> B.sqlBool_ (B.lower_ driver.firstName `B.like_` B.lower_ (B.val_ ("%" <> name <> "%")))) mbSearchString
                                   B.||?. maybe (B.sqlBool_ $ B.val_ True) (\lastDigits -> B.sqlBool_ (B.like_ (B.coalesce_ [driver.maskedMobileDigits] (B.val_ "")) (B.val_ ("%" <> takeEnd 4 lastDigits <> "%")))) mbSearchString
                                   B.||?. maybe (B.sqlBool_ $ B.val_ True) (\mobileNumberSearchStringDB -> driver.mobileNumberHash B.==?. B.val_ (Just mobileNumberSearchStringDB)) encryptedMobileNumberHash
                               )
                  )
                  do
                    fleetDriverAssociation <- B.all_ (BeamCommon.fleetDriverAssociation BeamCommon.atlasDB)
                    driver <- B.join_ (BeamCommon.person BeamCommon.atlasDB) (\driver -> BeamFDVA.driverId fleetDriverAssociation B.==. BeamP.id driver)
                    pure (fleetDriverAssociation, driver)
  case res of
    Right fleetDriverList -> catMaybes <$> mapM (\(f, d) -> liftA2 (,) <$> fromTType' f <*> fromTType' d) fleetDriverList
    Left _ -> pure []

findAllDriverByFleetOwnerIds :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r, EncFlow m r) => [Text] -> Maybe Int -> Maybe Int -> Maybe DbHash -> Maybe Text -> Maybe Text -> m [(FleetDriverAssociation, Person)]
findAllDriverByFleetOwnerIds fleetOwnerIds mbLimit mbOffset mbMobileNumberSearchStringHash mbName mbSearchString = do
  dbConf <- getReplicaBeamConfig
  now <- getCurrentTime
  encryptedMobileNumberHash <- mapM getDbHash mbSearchString
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (fromIntegral limit) $
            B.offset_ (fromIntegral offset) $
              B.orderBy_ (\(fleetDriverAssociation', _) -> B.desc_ fleetDriverAssociation'.updatedAt) $
                B.filter_'
                  ( \(fleetDriverAssociation, driver) ->
                      (B.sqlBool_ (fleetDriverAssociation.fleetOwnerId `B.in_` (B.val_ <$> fleetOwnerIds)))
                        B.&&?. B.sqlBool_ (fleetDriverAssociation.associatedTill B.>=. B.val_ (Just now))
                        B.&&?. (fleetDriverAssociation.isActive B.==?. B.val_ True)
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\name -> B.sqlBool_ (B.lower_ driver.firstName `B.like_` B.lower_ (B.val_ ("%" <> name <> "%")))) mbName
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\mobileNumberSearchStringDB -> driver.mobileNumberHash B.==?. B.val_ (Just mobileNumberSearchStringDB)) mbMobileNumberSearchStringHash
                        B.&&?. ( maybe (B.sqlBool_ $ B.val_ True) (\name -> B.sqlBool_ (B.lower_ driver.firstName `B.like_` B.lower_ (B.val_ ("%" <> name <> "%")))) mbSearchString
                                   B.||?. maybe (B.sqlBool_ $ B.val_ True) (\lastDigits -> B.sqlBool_ (B.like_ (B.coalesce_ [driver.maskedMobileDigits] (B.val_ "")) (B.val_ ("%" <> takeEnd 4 lastDigits <> "%")))) mbSearchString
                                   B.||?. maybe (B.sqlBool_ $ B.val_ True) (\mobileNumberSearchStringDB -> driver.mobileNumberHash B.==?. B.val_ (Just mobileNumberSearchStringDB)) encryptedMobileNumberHash
                               )
                  )
                  do
                    fleetDriverAssociation <- B.all_ (BeamCommon.fleetDriverAssociation BeamCommon.atlasDB)
                    driver <- B.join_ (BeamCommon.person BeamCommon.atlasDB) (\driver -> BeamFDVA.driverId fleetDriverAssociation B.==. BeamP.id driver)
                    pure (fleetDriverAssociation, driver)
  case res of
    Right fleetDriverList -> catMaybes <$> mapM (\(f, d) -> liftA2 (,) <$> fromTType' f <*> fromTType' d) fleetDriverList
    Left _ -> pure []

--------------------------------- multi fleet owner queries ----------------------------------

findAllActiveDriverByFleetOwnerIdWithDriverInfoMF :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r, EncFlow m r) => [Text] -> Int -> Int -> Maybe DbHash -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe DI.DriverMode -> m [(FleetDriverAssociation, Person, DriverInformation)]
findAllActiveDriverByFleetOwnerIdWithDriverInfoMF fleetOwnerIds limit offset mbMobileNumberSearchStringHash mbName mbSearchString mbIsActive mbMode = do
  now <- getCurrentTime
  dbConf <- getReplicaBeamConfig
  encryptedMobileNumberHash <- mapM getDbHash mbSearchString

  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (toInteger limit) $
            B.offset_ (toInteger offset) $
              B.orderBy_ (\(fleetDriverAssociation', _, _) -> B.desc_ fleetDriverAssociation'.updatedAt) $
                B.filter_'
                  ( \(fleetDriverAssociation, driver, driverInformation) ->
                      (B.sqlBool_ (fleetDriverAssociation.fleetOwnerId `B.in_` (B.val_ <$> fleetOwnerIds)))
                        B.&&?. B.sqlBool_ (fleetDriverAssociation.associatedTill B.>=. B.val_ (Just now))
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\mode -> driverInformation.mode B.==?. B.val_ (Just mode)) mbMode
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\isActive -> fleetDriverAssociation.isActive B.==?. B.val_ isActive) mbIsActive
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\name -> B.sqlBool_ (B.lower_ driver.firstName `B.like_` B.lower_ (B.val_ ("%" <> name <> "%")))) mbName
                        B.&&?. maybe (B.sqlBool_ $ B.val_ True) (\mobileNumberSearchStringDB -> driver.mobileNumberHash B.==?. B.val_ (Just mobileNumberSearchStringDB)) mbMobileNumberSearchStringHash
                        B.&&?. ( maybe (B.sqlBool_ $ B.val_ True) (\name -> B.sqlBool_ (B.lower_ driver.firstName `B.like_` (B.val_ ("%" <> toLower name <> "%")))) mbSearchString
                                   B.||?. maybe (B.sqlBool_ $ B.val_ True) (\lastDigits -> B.sqlBool_ (B.like_ (B.coalesce_ [driver.maskedMobileDigits] (B.val_ "")) (B.val_ ("%" <> takeEnd 4 lastDigits <> "%")))) mbSearchString
                                   B.||?. maybe (B.sqlBool_ $ B.val_ True) (\mobileNumberSearchStringDB -> driver.mobileNumberHash B.==?. B.val_ (Just mobileNumberSearchStringDB)) encryptedMobileNumberHash
                               )
                  )
                  do
                    fleetDriverAssociation <- B.all_ (BeamCommon.fleetDriverAssociation BeamCommon.atlasDB)
                    driver <- B.join_ (BeamCommon.person BeamCommon.atlasDB) (\driver -> BeamFDVA.driverId fleetDriverAssociation B.==. BeamP.id driver)
                    driverInformation <- B.join_ (BeamCommon.driverInformation BeamCommon.atlasDB) (\driverInfo -> BeamP.id driver B.==. BeamDI.driverId driverInfo)
                    pure (fleetDriverAssociation, driver, driverInformation)
  case res of
    Right res' -> do
      let fleetDriverList = (\(fleetDriverAssociation, driver, driverInformation) -> (fleetDriverAssociation, driver, driverInformation)) <$> res'
      catMaybes <$> mapM (\(f, d, di) -> liftA3 (,,) <$> fromTType' f <*> fromTType' d <*> fromTType' di) fleetDriverList
    Left _ -> pure []

approveFleetDriverAssociation ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Person ->
  Id Person ->
  Maybe Text ->
  m ()
approveFleetDriverAssociation driverId fleetOwnerId responseReason = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamFDVA.isActive True,
      Se.Set BeamFDVA.responseReason responseReason,
      Se.Set BeamFDVA.updatedAt now
    ]
    [Se.And [Se.Is BeamFDVA.driverId $ Se.Eq (driverId.getId), Se.Is BeamFDVA.fleetOwnerId $ Se.Eq fleetOwnerId.getId]]

rejectFleetDriverAssociation ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Person ->
  Id Person ->
  Maybe Text ->
  m ()
rejectFleetDriverAssociation driverId fleetOwnerId responseReason = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamFDVA.isActive False,
      Se.Set BeamFDVA.responseReason responseReason,
      Se.Set BeamFDVA.associatedTill (Just now),
      Se.Set BeamFDVA.updatedAt now
    ]
    [Se.And [Se.Is BeamFDVA.driverId $ Se.Eq (driverId.getId), Se.Is BeamFDVA.fleetOwnerId $ Se.Eq fleetOwnerId.getId]]

revokeFleetDriverAssociation ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Person ->
  Id Person ->
  m ()
revokeFleetDriverAssociation driverId fleetOwnerId = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamFDVA.associatedTill (Just now),
      Se.Set BeamFDVA.isActive False,
      Se.Set BeamFDVA.updatedAt now
    ]
    [Se.And [Se.Is BeamFDVA.driverId $ Se.Eq (driverId.getId), Se.Is BeamFDVA.fleetOwnerId $ Se.Eq fleetOwnerId.getId]]
