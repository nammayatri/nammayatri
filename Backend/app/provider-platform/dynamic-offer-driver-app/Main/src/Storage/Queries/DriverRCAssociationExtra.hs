module Storage.Queries.DriverRCAssociationExtra where

import Control.Applicative (liftA2)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (toLower)
import qualified Database.Beam as B
import Domain.Types.DriverRCAssociation as DRCA
import Domain.Types.FleetDriverAssociation as FleetDriverAssociation
import Domain.Types.Person (Person)
import Domain.Types.VehicleRegistrationCertificate
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude hiding (on)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Common as SBC
import qualified Storage.Beam.DriverRCAssociation as BeamDRCA
import qualified Storage.Beam.FleetDriverAssociation as BeamFDA
import qualified Storage.Beam.VehicleRegistrationCertificate as BeamVRC
import Storage.Queries.FleetDriverAssociation ()
import Storage.Queries.OrphanInstances.DriverRCAssociation ()
import Storage.Queries.VehicleRegistrationCertificate ()

-- Extra code goes here --
findAllByDriverId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Person ->
  m [(DriverRCAssociation, VehicleRegistrationCertificate)]
findAllByDriverId driverId = do
  rcAssocs <- getRcAssocs driverId Nothing
  regCerts <- getRegCerts rcAssocs
  return $ linkDriversRC rcAssocs regCerts

findAllActiveAndInactiveAssociationsByDriverId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Person ->
  m [(DriverRCAssociation, VehicleRegistrationCertificate)]
findAllActiveAndInactiveAssociationsByDriverId driverId = do
  now <- getCurrentTime
  rcAssocs <- getRcAssocs driverId (Just now)
  regCerts <- getRegCerts rcAssocs
  return $ linkDriversRC rcAssocs regCerts

findAllActiveAssociationByRCId ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id VehicleRegistrationCertificate ->
  m [DriverRCAssociation]
findAllActiveAssociationByRCId (Id rcId) = do
  now <- getCurrentTime
  findAllWithKV [Se.And [Se.Is BeamDRCA.rcId $ Se.Eq rcId, Se.Is BeamDRCA.associatedTill $ Se.GreaterThan $ Just now]]

linkDriversRC :: [DriverRCAssociation] -> [VehicleRegistrationCertificate] -> [(DriverRCAssociation, VehicleRegistrationCertificate)]
linkDriversRC rcAssocs regCerts = do
  let certHM = buildCertHM regCerts
   in mapMaybe (mapRCWithDriver certHM) rcAssocs

mapRCWithDriver :: HashMap.HashMap Text VehicleRegistrationCertificate -> DriverRCAssociation -> Maybe (DriverRCAssociation, VehicleRegistrationCertificate)
mapRCWithDriver certHM rcAssoc = do
  let rcId = rcAssoc.rcId.getId
  cert <- HashMap.lookup rcId certHM
  Just (rcAssoc, cert)

buildRcHM :: [DriverRCAssociation] -> HashMap.HashMap Text DriverRCAssociation
buildRcHM rcAssocs =
  HashMap.fromList $ map (\r -> (r.rcId.getId, r)) rcAssocs

buildCertHM :: [VehicleRegistrationCertificate] -> HashMap.HashMap Text VehicleRegistrationCertificate
buildCertHM regCerts =
  HashMap.fromList $ map (\r -> (r.id.getId, r)) regCerts

getRegCerts :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [DriverRCAssociation] -> m [VehicleRegistrationCertificate]
getRegCerts rcAssocs = findAllWithKV [Se.Is BeamVRC.id $ Se.In $ getId <$> fetchRcIdFromAssocs rcAssocs]

fetchRcIdFromAssocs :: [DriverRCAssociation] -> [Id VehicleRegistrationCertificate]
fetchRcIdFromAssocs = map (.rcId)

getRcAssocs :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Maybe UTCTime -> m [DriverRCAssociation]
getRcAssocs (Id driverId) mbNow =
  findAllWithOptionsKV
    [ Se.And
        ( [Se.Is BeamDRCA.driverId $ Se.Eq driverId]
            <> [Se.Is BeamDRCA.associatedTill $ Se.GreaterThan $ mbNow | isJust mbNow]
        )
    ]
    (Se.Desc BeamDRCA.associatedOn)
    Nothing
    Nothing

endAssociationForRC :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Id VehicleRegistrationCertificate -> m ()
endAssociationForRC (Id driverId) (Id rcId) = do
  now <- getCurrentTime
  updateWithKV
    [Se.Set BeamDRCA.associatedTill $ Just now, Se.Set BeamDRCA.isRcActive False]
    [Se.And [Se.Is BeamDRCA.driverId (Se.Eq driverId), Se.Is BeamDRCA.associatedTill (Se.GreaterThan $ Just now), Se.Is BeamDRCA.rcId (Se.Eq rcId)]]

activateRCForDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Id VehicleRegistrationCertificate -> UTCTime -> m ()
activateRCForDriver (Id driverId) (Id rcId) now = updateWithKV [Se.Set BeamDRCA.isRcActive True] [Se.And [Se.Is BeamDRCA.driverId $ Se.Eq driverId, Se.Is BeamDRCA.rcId $ Se.Eq rcId, Se.Is BeamDRCA.associatedTill $ Se.GreaterThan $ Just now]]

findLinkedByRCIdAndDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Id VehicleRegistrationCertificate -> UTCTime -> m (Maybe DriverRCAssociation)
findLinkedByRCIdAndDriverId (Id driverId) (Id rcId) now = findOneWithKV [Se.And [Se.Is BeamDRCA.driverId $ Se.Eq driverId, Se.Is BeamDRCA.rcId $ Se.Eq rcId, Se.Is BeamDRCA.associatedTill $ Se.GreaterThan $ Just now]]

updateRcErrorMessage :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Id VehicleRegistrationCertificate -> Text -> m ()
updateRcErrorMessage (Id driverId) (Id rcId) errorMessage = updateWithKV [Se.Set BeamDRCA.errorMessage (Just errorMessage)] [Se.And [Se.Is BeamDRCA.driverId $ Se.Eq driverId, Se.Is BeamDRCA.rcId $ Se.Eq rcId]]

findLatestByRCIdAndDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id VehicleRegistrationCertificate -> Id Person -> m (Maybe DriverRCAssociation)
findLatestByRCIdAndDriverId (Id rcId) (Id driverId) =
  findAllWithOptionsKV [Se.And [Se.Is BeamDRCA.rcId $ Se.Eq rcId, Se.Is BeamDRCA.driverId $ Se.Eq driverId]] (Se.Desc BeamDRCA.associatedTill) (Just 1) Nothing <&> listToMaybe

findLatestLinkedByRCId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id VehicleRegistrationCertificate -> UTCTime -> m (Maybe DriverRCAssociation)
findLatestLinkedByRCId (Id rcId) now =
  findAllWithOptionsKV [Se.And [Se.Is BeamDRCA.rcId $ Se.Eq rcId, Se.Is BeamDRCA.associatedTill $ Se.GreaterThan $ Just now]] (Se.Desc BeamDRCA.associatedOn) (Just 1) Nothing <&> listToMaybe

findLatestLinkedByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> UTCTime -> m (Maybe DriverRCAssociation)
findLatestLinkedByDriverId (Id driverId) now =
  findAllWithOptionsKV [Se.And [Se.Is BeamDRCA.driverId $ Se.Eq driverId, Se.Is BeamDRCA.associatedTill $ Se.GreaterThan $ Just now]] (Se.Desc BeamDRCA.associatedOn) (Just 1) Nothing <&> listToMaybe

findAllLinkedByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m [DriverRCAssociation]
findAllLinkedByDriverId (Id driverId) = do
  now <- getCurrentTime
  findAllWithOptionsKV [Se.Is BeamDRCA.driverId $ Se.Eq driverId, Se.Is BeamDRCA.associatedTill $ Se.GreaterThan $ Just now] (Se.Desc BeamDRCA.associatedOn) Nothing Nothing

findUnlinkedRC :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Id VehicleRegistrationCertificate -> m [DriverRCAssociation]
findUnlinkedRC (Id driverId) (Id rcId) = do
  now <- getCurrentTime
  findAllWithOptionsKV [Se.Is BeamDRCA.driverId $ Se.Eq driverId, Se.Is BeamDRCA.rcId $ Se.Eq rcId, Se.Is BeamDRCA.associatedTill $ Se.LessThan $ Just now] (Se.Desc BeamDRCA.associatedOn) Nothing Nothing

mapping :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Text -> Maybe Int -> Maybe Int -> m [(VehicleRegistrationCertificate, FleetDriverAssociation, DriverRCAssociation)]
mapping fleetIdWanted mbLimit mbOffset = do
  dbConf <- getReplicaBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.limit_ (fromIntegral $ fromMaybe 100 mbLimit) $
          B.offset_ (fromIntegral $ fromMaybe 0 mbOffset) $
            do
              vehicleRC' <- B.filter_' (\vehicleRC'' -> BeamVRC.fleetOwnerId vehicleRC'' B.==?. B.val_ fleetIdWanted) (B.all_ (SBC.vehicleRegistrationCertificate SBC.atlasDB))
              fleetDriverAssn' <- B.join_' (SBC.fleetDriverAssociation SBC.atlasDB) (\fleetDriverAssn'' -> fleetDriverAssn''.fleetOwnerId B.==?. B.val_ (fromMaybe "" fleetIdWanted))
              driverRCAssn' <- B.join_' (SBC.driverRCAssociation SBC.atlasDB) (\driverRCAssn'' -> BeamDRCA.driverId driverRCAssn'' B.==?. BeamFDA.driverId fleetDriverAssn')
              pure (vehicleRC', fleetDriverAssn', driverRCAssn')
  case res of
    Right res' -> do
      let vrc' = fmap fst' res'
          fda' = fmap snd' res'
          drca' = fmap thd' res'
      vrc <- catMaybes <$> mapM fromTType' vrc'
      fda <- catMaybes <$> mapM fromTType' fda'
      drca <- catMaybes <$> mapM fromTType' drca'
      pure $ zip3 vrc fda drca
    Left _ -> pure []
  where
    fst' (x, _, _) = x
    snd' (_, y, _) = y
    thd' (_, _, z) = z

-- Returns a structured result to facilitate association checks
findValidAssociationsForDriverOrRC ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id Person ->
  Id VehicleRegistrationCertificate ->
  UTCTime ->
  m [DriverRCAssociation]
findValidAssociationsForDriverOrRC (Id driverId) (Id rcId) now = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Or
            [ Se.Is BeamDRCA.driverId $ Se.Eq driverId,
              Se.Is BeamDRCA.rcId $ Se.Eq rcId
            ],
          Se.Is BeamDRCA.associatedTill $ Se.GreaterThan $ Just now
        ]
    ]
    (Se.Desc BeamDRCA.associatedOn)
    Nothing
    Nothing

findAllActiveAssociationByFleetOwnerId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe DbHash -> m [(DriverRCAssociation, VehicleRegistrationCertificate)]
findAllActiveAssociationByFleetOwnerId fleetOwnerId Nothing Nothing mbRegNumberString mbRegNumberStringHash = do
  now <- getCurrentTime
  dbConf <- getReplicaBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.filter_'
          ( \(rcAssn, rc) ->
              rcAssn.isRcActive B.==?. B.val_ True
                B.&&?. B.sqlBool_ (rcAssn.associatedTill B.>=. B.val_ (Just now))
                B.&&?. rc.fleetOwnerId B.==?. B.val_ (Just fleetOwnerId)
                B.&&?. ( maybe
                           (B.sqlBool_ $ B.val_ True)
                           (\cNum -> B.sqlBool_ (B.like_ (B.lower_ (B.coalesce_ [rc.unencryptedCertificateNumber] (B.val_ ""))) (B.val_ ("%" <> toLower cNum <> "%"))))
                           mbRegNumberString
                           B.||?. maybe
                             (B.sqlBool_ $ B.val_ True)
                             (\searchStrDBHash -> rc.certificateNumberHash B.==?. B.val_ searchStrDBHash)
                             mbRegNumberStringHash
                       )
          )
          $ do
            rcAssn <- B.all_ (SBC.driverRCAssociation SBC.atlasDB)
            rc <- B.join_ (SBC.vehicleRegistrationCertificate SBC.atlasDB) (\vrc -> BeamVRC.id vrc B.==. BeamDRCA.rcId rcAssn)
            pure (rcAssn, rc)
  case res of
    Right rows ->
      catMaybes <$> mapM (\(rc, vrc) -> liftA2 (,) <$> fromTType' rc <*> fromTType' vrc) rows
    Left _ -> pure []
findAllActiveAssociationByFleetOwnerId fleetOwnerId mbLimit mbOffset mbRegNumberString mbRegNumberStringHash = do
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  now <- getCurrentTime
  dbConf <- getReplicaBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.limit_ (fromIntegral limit) $
          B.offset_ (fromIntegral offset) $
            B.filter_'
              ( \(rcAssn, rc) ->
                  rcAssn.isRcActive B.==?. B.val_ True
                    B.&&?. B.sqlBool_ (rcAssn.associatedTill B.>=. B.val_ (Just now))
                    B.&&?. rc.fleetOwnerId B.==?. B.val_ (Just fleetOwnerId)
                    B.&&?. ( maybe
                               (B.sqlBool_ $ B.val_ True)
                               (\cNum -> B.sqlBool_ (B.like_ (B.lower_ (B.coalesce_ [rc.unencryptedCertificateNumber] (B.val_ ""))) (B.val_ ("%" <> toLower cNum <> "%"))))
                               mbRegNumberString
                               B.||?. maybe
                                 (B.sqlBool_ $ B.val_ True)
                                 (\searchStrDBHash -> rc.certificateNumberHash B.==?. B.val_ searchStrDBHash)
                                 mbRegNumberStringHash
                           )
              )
              $ do
                rcAssn <- B.all_ (SBC.driverRCAssociation SBC.atlasDB)
                rc <- B.join_ (SBC.vehicleRegistrationCertificate SBC.atlasDB) (\vrc -> BeamVRC.id vrc B.==. BeamDRCA.rcId rcAssn)
                pure (rcAssn, rc)
  case res of
    Right rows ->
      catMaybes <$> mapM (\(rc, vrc) -> liftA2 (,) <$> fromTType' rc <*> fromTType' vrc) rows
    Left _ -> pure []

findAllInactiveAssociationByFleetOwnerId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> Int -> Int -> Maybe Text -> Maybe DbHash -> m [(DriverRCAssociation, VehicleRegistrationCertificate)]
findAllInactiveAssociationByFleetOwnerId fleetOwnerId limit offset mbRegNumberString mbRegNumberStringHash = do
  allActiveAssocs <- findAllActiveAssociationByFleetOwnerId fleetOwnerId Nothing Nothing mbRegNumberString mbRegNumberStringHash
  let allActiveRcIds = map (rcId . fst) allActiveAssocs
  dbConf <- getReplicaBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.limit_ (fromIntegral limit) $
          B.offset_ (fromIntegral offset) $
            B.filter_'
              ( \(rcAssn, rc) ->
                  rc.fleetOwnerId B.==?. B.val_ (Just fleetOwnerId)
                    B.&&?. B.sqlBool_ (B.not_ (rcAssn.rcId `B.in_` (B.val_ . getId <$> allActiveRcIds)))
                    B.&&?. ( maybe
                               (B.sqlBool_ $ B.val_ True)
                               (\cNum -> B.sqlBool_ (B.like_ (B.lower_ (B.coalesce_ [rc.unencryptedCertificateNumber] (B.val_ ""))) (B.val_ ("%" <> toLower cNum <> "%"))))
                               mbRegNumberString
                               B.||?. maybe
                                 (B.sqlBool_ $ B.val_ True)
                                 (\searchStrDBHash -> rc.certificateNumberHash B.==?. B.val_ searchStrDBHash)
                                 mbRegNumberStringHash
                           )
              )
              $ do
                rcAssn <- B.all_ (SBC.driverRCAssociation SBC.atlasDB)
                rc <- B.join_ (SBC.vehicleRegistrationCertificate SBC.atlasDB) (\vrc -> BeamVRC.id vrc B.==. BeamDRCA.rcId rcAssn)
                pure (rcAssn, rc)
  case res of
    Right rows ->
      catMaybes <$> mapM (\(rc, vrc) -> liftA2 (,) <$> fromTType' rc <*> fromTType' vrc) rows
    Left _ -> pure []

---------------------------- Various queries with array of fleet owner ids ----------------------------

findAllActiveAssociationByFleetOwnerIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Text] -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe DbHash -> m [(DriverRCAssociation, VehicleRegistrationCertificate)]
findAllActiveAssociationByFleetOwnerIds fleetOwnerIds Nothing Nothing mbRegNumberString mbRegNumberStringHash = do
  now <- getCurrentTime
  dbConf <- getReplicaBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.filter_'
          ( \(rcAssn, rc) ->
              rcAssn.isRcActive B.==?. B.val_ True
                B.&&?. B.sqlBool_ (rcAssn.associatedTill B.>=. B.val_ (Just now))
                B.&&?. B.sqlBool_ (rc.fleetOwnerId `B.in_` ((B.val_ . Just) <$> fleetOwnerIds))
                B.&&?. ( maybe
                           (B.sqlBool_ $ B.val_ True)
                           (\cNum -> B.sqlBool_ (B.like_ (B.lower_ (B.coalesce_ [rc.unencryptedCertificateNumber] (B.val_ ""))) (B.val_ ("%" <> toLower cNum <> "%"))))
                           mbRegNumberString
                           B.||?. maybe
                             (B.sqlBool_ $ B.val_ True)
                             (\searchStrDBHash -> rc.certificateNumberHash B.==?. B.val_ searchStrDBHash)
                             mbRegNumberStringHash
                       )
          )
          $ do
            rcAssn <- B.all_ (SBC.driverRCAssociation SBC.atlasDB)
            rc <- B.join_ (SBC.vehicleRegistrationCertificate SBC.atlasDB) (\vrc -> BeamVRC.id vrc B.==. BeamDRCA.rcId rcAssn)
            pure (rcAssn, rc)
  case res of
    Right rows ->
      catMaybes <$> mapM (\(rc, vrc) -> liftA2 (,) <$> fromTType' rc <*> fromTType' vrc) rows
    Left _ -> pure []
findAllActiveAssociationByFleetOwnerIds fleetOwnerIds mbLimit mbOffset mbRegNumberString mbRegNumberStringHash = do
  let limit = fromMaybe 10 mbLimit
      offset = fromMaybe 0 mbOffset
  now <- getCurrentTime
  dbConf <- getReplicaBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.limit_ (fromIntegral limit) $
          B.offset_ (fromIntegral offset) $
            B.filter_'
              ( \(rcAssn, rc) ->
                  rcAssn.isRcActive B.==?. B.val_ True
                    B.&&?. B.sqlBool_ (rcAssn.associatedTill B.>=. B.val_ (Just now))
                    B.&&?. B.sqlBool_ (rc.fleetOwnerId `B.in_` ((B.val_ . Just) <$> fleetOwnerIds))
                    B.&&?. ( maybe
                               (B.sqlBool_ $ B.val_ True)
                               (\cNum -> B.sqlBool_ (B.like_ (B.lower_ (B.coalesce_ [rc.unencryptedCertificateNumber] (B.val_ ""))) (B.val_ ("%" <> toLower cNum <> "%"))))
                               mbRegNumberString
                               B.||?. maybe
                                 (B.sqlBool_ $ B.val_ True)
                                 (\searchStrDBHash -> rc.certificateNumberHash B.==?. B.val_ searchStrDBHash)
                                 mbRegNumberStringHash
                           )
              )
              $ do
                rcAssn <- B.all_ (SBC.driverRCAssociation SBC.atlasDB)
                rc <- B.join_ (SBC.vehicleRegistrationCertificate SBC.atlasDB) (\vrc -> BeamVRC.id vrc B.==. BeamDRCA.rcId rcAssn)
                pure (rcAssn, rc)
  case res of
    Right rows ->
      catMaybes <$> mapM (\(rc, vrc) -> liftA2 (,) <$> fromTType' rc <*> fromTType' vrc) rows
    Left _ -> pure []

findAllInactiveAssociationByFleetOwnerIds :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Text] -> Int -> Int -> Maybe Text -> Maybe DbHash -> m [(DriverRCAssociation, VehicleRegistrationCertificate)]
findAllInactiveAssociationByFleetOwnerIds fleetOwnerIds limit offset mbRegNumberString mbRegNumberStringHash = do
  allActiveAssocs <- findAllActiveAssociationByFleetOwnerIds fleetOwnerIds Nothing Nothing mbRegNumberString mbRegNumberStringHash
  let allActiveRcIds = map (rcId . fst) allActiveAssocs
  dbConf <- getReplicaBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.limit_ (fromIntegral limit) $
          B.offset_ (fromIntegral offset) $
            B.filter_'
              ( \(rcAssn, rc) ->
                  B.sqlBool_ (rc.fleetOwnerId `B.in_` (B.val_ . Just <$> fleetOwnerIds))
                    B.&&?. B.sqlBool_ (B.not_ (rcAssn.rcId `B.in_` (B.val_ . getId <$> allActiveRcIds)))
                    B.&&?. ( maybe
                               (B.sqlBool_ $ B.val_ True)
                               (\cNum -> B.sqlBool_ (B.like_ (B.lower_ (B.coalesce_ [rc.unencryptedCertificateNumber] (B.val_ ""))) (B.val_ ("%" <> toLower cNum <> "%"))))
                               mbRegNumberString
                               B.||?. maybe
                                 (B.sqlBool_ $ B.val_ True)
                                 (\searchStrDBHash -> rc.certificateNumberHash B.==?. B.val_ searchStrDBHash)
                                 mbRegNumberStringHash
                           )
              )
              $ do
                rcAssn <- B.all_ (SBC.driverRCAssociation SBC.atlasDB)
                rc <- B.join_ (SBC.vehicleRegistrationCertificate SBC.atlasDB) (\vrc -> BeamVRC.id vrc B.==. BeamDRCA.rcId rcAssn)
                pure (rcAssn, rc)
  case res of
    Right rows ->
      catMaybes <$> mapM (\(rc, vrc) -> liftA2 (,) <$> fromTType' rc <*> fromTType' vrc) rows
    Left _ -> pure []
