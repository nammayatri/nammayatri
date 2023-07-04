{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Storage.Queries.DriverOnboarding.DriverRCAssociation where

import qualified Data.HashMap.Strict as HashMap
import Domain.Types.DriverOnboarding.DriverRCAssociation as DRCA
import Domain.Types.DriverOnboarding.VehicleRegistrationCertificate
import Domain.Types.Person (Person)
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude hiding (on)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Utils (setMeshConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOnboarding.DriverRCAssociation as BeamDRCA
import qualified Storage.Beam.DriverOnboarding.VehicleRegistrationCertificate as BeamVRC
import Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate as QVRC

create :: L.MonadFlow m => DriverRCAssociation -> m ()
create driverRCAssociation = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDRCA.DriverRCAssociationT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> void $ KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainDriverRCAssociationToBeam driverRCAssociation)
    Nothing -> pure ()

findById :: L.MonadFlow m => Id DriverRCAssociation -> m (Maybe DriverRCAssociation)
findById (Id drcaId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDRCA.DriverRCAssociationT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamDriverRCAssociationToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamDRCA.id $ Se.Eq drcaId]
    Nothing -> pure Nothing

getActiveAssociationByDriver :: (L.MonadFlow m, MonadTime m) => Id Person -> m (Maybe DriverRCAssociation)
getActiveAssociationByDriver (Id personId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDRCA.DriverRCAssociationT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamDriverRCAssociationToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.And [Se.Is BeamDRCA.driverId $ Se.Eq personId, Se.Is BeamDRCA.associatedTill $ Se.GreaterThan $ Just now]]
    Nothing -> pure Nothing

-- findAllByDriverId ::
--   Transactionable m =>
--   Id Person ->
--   m [(DriverRCAssociation, VehicleRegistrationCertificate)]
-- findAllByDriverId driverId = do
--   rcAssocs <- getRcAssocs driverId
--   regCerts <- getRegCerts rcAssocs
--   return $ linkDriversRC rcAssocs regCerts

findAllByDriverId ::
  L.MonadFlow m =>
  Id Person ->
  m [(DriverRCAssociation, VehicleRegistrationCertificate)]
findAllByDriverId driverId = do
  rcAssocs <- getRcAssocs driverId
  regCerts <- getRegCerts rcAssocs
  return $ linkDriversRC rcAssocs regCerts

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

-- getRegCerts ::
--   Transactionable m =>
--   [DriverRCAssociation] ->
--   m [VehicleRegistrationCertificate]
-- getRegCerts rcAssocs = do
--   Esq.findAll $ do
--     regCerts <- from $ table @VehicleRegistrationCertificateT
--     where_ $
--       regCerts ^. VehicleRegistrationCertificateTId `in_` valList rcAssocsKeys
--     return regCerts
--   where
--     rcAssocsKeys = toKey . cast <$> fetchRcIdFromAssocs rcAssocs

getRegCerts :: L.MonadFlow m => [DriverRCAssociation] -> m [VehicleRegistrationCertificate]
getRegCerts rcAssocs = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamVRC.VehicleRegistrationCertificateT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <-
        KV.findAllWithKVConnector
          dbConf'
          updatedMeshConfig
          [Se.Is BeamVRC.id $ Se.In $ getId <$> rcAssocsKeys]
      case result of
        Left _ -> pure []
        Right result' -> pure $ QVRC.transformBeamVehicleRegistrationCertificateToDomain <$> result'
    Nothing -> pure []
  where
    rcAssocsKeys = fetchRcIdFromAssocs rcAssocs

fetchRcIdFromAssocs :: [DriverRCAssociation] -> [Id VehicleRegistrationCertificate]
fetchRcIdFromAssocs = map (.rcId)

-- getRcAssocs ::
--   Transactionable m =>
--   Id Person ->
--   m [DriverRCAssociation]
-- getRcAssocs driverId = do
--   Esq.findAll $ do
--     rcAssoc <- from $ table @DriverRCAssociationT
--     where_ $
--       rcAssoc ^. DriverRCAssociationDriverId ==. val (toKey driverId)
--     orderBy [desc $ rcAssoc ^. DriverRCAssociationAssociatedOn]
--     return rcAssoc

getRcAssocs :: L.MonadFlow m => Id Person -> m [DriverRCAssociation]
getRcAssocs (Id driverId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDRCA.DriverRCAssociationT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <-
        KV.findAllWithOptionsKVConnector
          dbConf'
          updatedMeshConfig
          [Se.Is BeamDRCA.driverId $ Se.Eq driverId]
          (Se.Desc BeamDRCA.associatedOn)
          Nothing
          Nothing
      case result of
        Left _ -> pure []
        Right result' -> pure $ transformBeamDriverRCAssociationToDomain <$> result'
    Nothing -> pure []

getActiveAssociationByRC :: (L.MonadFlow m, MonadTime m) => Id VehicleRegistrationCertificate -> m (Maybe DriverRCAssociation)
getActiveAssociationByRC (Id rcId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDRCA.DriverRCAssociationT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamDriverRCAssociationToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.And [Se.Is BeamDRCA.driverId $ Se.Eq rcId, Se.Is BeamDRCA.associatedTill $ Se.GreaterThan $ Just now]]
    Nothing -> pure Nothing

endAssociation :: (L.MonadFlow m, MonadTime m) => Id Person -> m ()
endAssociation (Id driverId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDRCA.DriverRCAssociationT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      void $
        KV.updateWoReturningWithKVConnector
          dbConf'
          updatedMeshConfig
          [ Se.Set BeamDRCA.associatedTill $ Just now
          ]
          [Se.And [Se.Is BeamDRCA.id (Se.Eq driverId), Se.Is BeamDRCA.associatedTill (Se.GreaterThan $ Just now)]]
    Nothing -> pure ()

deleteByDriverId :: L.MonadFlow m => Id Person -> m ()
deleteByDriverId (Id driverId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDRCA.DriverRCAssociationT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' ->
      void $
        KV.deleteWithKVConnector
          dbConf'
          updatedMeshConfig
          [Se.Is BeamDRCA.driverId (Se.Eq driverId)]
    Nothing -> pure ()

transformBeamDriverRCAssociationToDomain :: BeamDRCA.DriverRCAssociation -> DriverRCAssociation
transformBeamDriverRCAssociationToDomain BeamDRCA.DriverRCAssociationT {..} = do
  DriverRCAssociation
    { id = Id id,
      driverId = Id driverId,
      rcId = Id rcId,
      associatedOn = associatedOn,
      associatedTill = associatedTill,
      consent = consent,
      consentTimestamp = consentTimestamp
    }

transformDomainDriverRCAssociationToBeam :: DriverRCAssociation -> BeamDRCA.DriverRCAssociation
transformDomainDriverRCAssociationToBeam DriverRCAssociation {..} =
  BeamDRCA.DriverRCAssociationT
    { BeamDRCA.id = getId id,
      BeamDRCA.driverId = getId driverId,
      BeamDRCA.rcId = getId rcId,
      BeamDRCA.associatedOn = associatedOn,
      BeamDRCA.associatedTill = associatedTill,
      BeamDRCA.consent = consent,
      BeamDRCA.consentTimestamp = consentTimestamp
    }
