module Storage.Queries.DriverRCAssociationExtra where

import qualified Data.HashMap.Strict as HashMap
import qualified Database.Beam as B
import Domain.Types.DriverRCAssociation as DRCA
import Domain.Types.FleetDriverAssociation as FleetDriverAssociation
import Domain.Types.Person (Person)
import Domain.Types.VehicleRegistrationCertificate
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
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
  rcAssocs <- getRcAssocs driverId
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

getRcAssocs :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m [DriverRCAssociation]
getRcAssocs (Id driverId) = findAllWithOptionsKV [Se.Is BeamDRCA.driverId $ Se.Eq driverId] (Se.Desc BeamDRCA.associatedOn) Nothing Nothing

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

findLatestByRCIdAndDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id VehicleRegistrationCertificate -> Id Person -> m (Maybe DriverRCAssociation)
findLatestByRCIdAndDriverId (Id rcId) (Id driverId) =
  findAllWithOptionsKV [Se.And [Se.Is BeamDRCA.rcId $ Se.Eq rcId, Se.Is BeamDRCA.driverId $ Se.Eq driverId]] (Se.Desc BeamDRCA.associatedTill) (Just 1) Nothing <&> listToMaybe

findLatestLinkedByRCId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id VehicleRegistrationCertificate -> UTCTime -> m (Maybe DriverRCAssociation)
findLatestLinkedByRCId (Id rcId) now =
  findAllWithOptionsKV [Se.And [Se.Is BeamDRCA.rcId $ Se.Eq rcId, Se.Is BeamDRCA.associatedTill $ Se.GreaterThan $ Just now]] (Se.Desc BeamDRCA.associatedOn) (Just 1) Nothing <&> listToMaybe

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
