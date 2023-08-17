{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverOnboarding.DriverRCAssociation where

import qualified Data.HashMap.Strict as HashMap
import Domain.Types.DriverOnboarding.DriverRCAssociation as DRCA
import Domain.Types.DriverOnboarding.VehicleRegistrationCertificate
import Domain.Types.Person (Person)
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude hiding (on)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.DriverOnboarding.DriverRCAssociation as BeamDRCA
import qualified Storage.Beam.DriverOnboarding.VehicleRegistrationCertificate as BeamVRC
import Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate ()

create :: (L.MonadFlow m, Log m) => DriverRCAssociation -> m ()
create = createWithKV

findById :: (L.MonadFlow m, Log m) => Id DriverRCAssociation -> m (Maybe DriverRCAssociation)
findById (Id drcaId) = findOneWithKV [Se.Is BeamDRCA.id $ Se.Eq drcaId]

-- getActiveAssociationByDriver :: (L.MonadFlow m, MonadTime m, Log m) => Id Person -> m (Maybe DriverRCAssociation)
-- getActiveAssociationByDriver (Id personId) = do
--   now <- getCurrentTime
--   findOneWithKV [Se.And [Se.Is BeamDRCA.driverId $ Se.Eq personId, Se.Is BeamDRCA.associatedTill $ Se.GreaterThan $ Just now]]

-- findAllByDriverId ::
--   Transactionable m =>
--   Id Person ->
--   m [(DriverRCAssociation, VehicleRegistrationCertificate)]
-- findAllByDriverId driverId = do
--   rcAssocs <- getRcAssocs driverId
--   regCerts <- getRegCerts rcAssocs
--   return $ linkDriversRC rcAssocs regCerts

findAllByDriverId ::
  (L.MonadFlow m, Log m) =>
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
--     return regCerts
--   where
--     rcAssocsKeys = toKey . cast <$> fetchRcIdFromAssocs rcAssocs

getRegCerts :: (L.MonadFlow m, Log m) => [DriverRCAssociation] -> m [VehicleRegistrationCertificate]
getRegCerts rcAssocs = findAllWithKV [Se.Is BeamVRC.id $ Se.In $ getId <$> fetchRcIdFromAssocs rcAssocs]

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

getRcAssocs :: (L.MonadFlow m, Log m) => Id Person -> m [DriverRCAssociation]
getRcAssocs (Id driverId) = findAllWithOptionsKV [Se.Is BeamDRCA.driverId $ Se.Eq driverId] (Se.Desc BeamDRCA.associatedOn) Nothing Nothing

-- getActiveAssociationByRC :: (L.MonadFlow m, MonadTime m, Log m) => Id VehicleRegistrationCertificate -> m (Maybe DriverRCAssociation)
-- getActiveAssociationByRC (Id rcId) = do
--   now <- getCurrentTime
--   findOneWithKV [Se.And [Se.Is BeamDRCA.driverId $ Se.Eq rcId, Se.Is BeamDRCA.associatedTill $ Se.GreaterThan $ Just now]]

endAssociationForRC :: (L.MonadFlow m, MonadTime m, Log m) => Id Person -> Id VehicleRegistrationCertificate -> m ()
endAssociationForRC (Id driverId) (Id rcId) = do
  now <- getCurrentTime
  updateWithKV
    [Se.Set BeamDRCA.associatedTill $ Just now]
    [Se.And [Se.Is BeamDRCA.driverId (Se.Eq driverId), Se.Is BeamDRCA.associatedTill (Se.GreaterThan $ Just now), Se.Is BeamDRCA.rcId (Se.Eq rcId)]]

deleteByDriverId :: (L.MonadFlow m, Log m) => Id Person -> m ()
deleteByDriverId (Id driverId) = deleteWithKV [Se.Is BeamDRCA.driverId (Se.Eq driverId)]

instance FromTType' BeamDRCA.DriverRCAssociation DriverRCAssociation where
  fromTType' BeamDRCA.DriverRCAssociationT {..} = do
    pure $
      Just
        DriverRCAssociation
          { id = Id id,
            driverId = Id driverId,
            rcId = Id rcId,
            associatedOn = associatedOn,
            associatedTill = associatedTill,
            consent = consent,
            consentTimestamp = consentTimestamp,
            isRcActive = isRcActive
          }

instance ToTType' BeamDRCA.DriverRCAssociation DriverRCAssociation where
  toTType' DriverRCAssociation {..} = do
    BeamDRCA.DriverRCAssociationT
      { BeamDRCA.id = getId id,
        BeamDRCA.driverId = getId driverId,
        BeamDRCA.rcId = getId rcId,
        BeamDRCA.associatedOn = associatedOn,
        BeamDRCA.associatedTill = associatedTill,
        BeamDRCA.consent = consent,
        BeamDRCA.consentTimestamp = consentTimestamp,
        BeamDRCA.isRcActive = isRcActive
      }

findActiveAssociationByRC :: (L.MonadFlow m, Log m) => Id VehicleRegistrationCertificate -> m (Maybe DriverRCAssociation)
findActiveAssociationByRC (Id rcId) = findOneWithKV [Se.And [Se.Is BeamDRCA.rcId $ Se.Eq rcId, Se.Is BeamDRCA.isRcActive $ Se.Eq True]]

-- findActiveAssociationByDriver ::
--   Transactionable m =>
--   Id Person ->
--   m (Maybe DriverRCAssociation)
-- findActiveAssociationByDriver driverId = do
--   findOne $ do
--     association <- from $ table @DriverRCAssociationT
--     where_ $
--       association ^. DriverRCAssociationDriverId ==. val (toKey driverId)
--         &&. association ^. DriverRCAssociationIsRcActive ==. val True
--     return association

-- findActiveAssociationByDriver ::
--   (L.MonadFlow m) =>
--   Id Person ->
--   m (Maybe DriverRCAssociation)
-- findActiveAssociationByDriver (Id driverId) = findOneWithKV [Se.And [Se.Is BeamDRCA.driverId $ Se.Eq driverId, Se.Is BeamDRCA.isRcActive $ Se.Eq True]]
--   findOne $ do
--     association <- from $ table @DriverRCAssociationT
--     where_ $
--       association ^. DriverRCAssociationDriverId ==. val (toKey driverId)
--         &&. association ^. DriverRCAssociationIsRcActive ==. val True
--     return association

findActiveAssociationByDriver :: (L.MonadFlow m, Log m) => Id Person -> m (Maybe DriverRCAssociation)
findActiveAssociationByDriver (Id driverId) = findOneWithKV [Se.And [Se.Is BeamDRCA.driverId $ Se.Eq driverId, Se.Is BeamDRCA.isRcActive $ Se.Eq True]]

-- deactivateRCForDriver :: Id Person -> Id VehicleRegistrationCertificate -> SqlDB ()
-- deactivateRCForDriver driverId rcId = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverRCAssociationIsRcActive =. val False
--       ]
--     where_ $
--       tbl ^. DriverRCAssociationDriverId ==. val (toKey driverId)
--         &&. tbl ^. DriverRCAssociationRcId ==. val (toKey rcId)

deactivateRCForDriver :: (L.MonadFlow m, Log m) => Id Person -> Id VehicleRegistrationCertificate -> m ()
deactivateRCForDriver (Id driverId) (Id rcId) = updateWithKV [Se.Set BeamDRCA.isRcActive False] [Se.And [Se.Is BeamDRCA.driverId $ Se.Eq driverId, Se.Is BeamDRCA.rcId $ Se.Eq rcId]]

activateRCForDriver :: (L.MonadFlow m, Log m) => Id Person -> Id VehicleRegistrationCertificate -> UTCTime -> m ()
activateRCForDriver (Id driverId) (Id rcId) now = updateWithKV [Se.Set BeamDRCA.isRcActive True] [Se.And [Se.Is BeamDRCA.driverId $ Se.Eq driverId, Se.Is BeamDRCA.rcId $ Se.Eq rcId, Se.Is BeamDRCA.associatedTill $ Se.GreaterThan $ Just now]]

-- findLinkedByRCIdAndDriverId :: Transactionable m => Id Person -> Id VehicleRegistrationCertificate -> UTCTime -> m (Maybe DriverRCAssociation)
-- findLinkedByRCIdAndDriverId driverId rcId now = do
--   Esq.findOne $ do
--     res <- from $ table @DriverRCAssociationT
--     where_ $
--       res ^. DriverRCAssociationDriverId ==. val (toKey driverId)
--         &&. res ^. DriverRCAssociationRcId ==. val (toKey rcId)
--         &&. res ^. DriverRCAssociationAssociatedTill >. val (Just now)
--     return res

-- findLatestByRCIdAndDriverId :: Transactionable m => Id VehicleRegistrationCertificate -> Id Person -> m (Maybe DriverRCAssociation)
-- findLatestByRCIdAndDriverId rcId driverId =
--   Esq.findOne $ do
--     rcAssoc <- from $ table @DriverRCAssociationT
--     where_ $
--       rcAssoc ^. DriverRCAssociationRcId ==. val (toKey rcId)
--         &&. rcAssoc ^. DriverRCAssociationDriverId ==. val (toKey driverId)
--     orderBy [desc $ rcAssoc ^. DriverRCAssociationAssociatedTill]
--     limit 1
--     return rcAssoc

-- findAllLinkedByDriverId :: (Transactionable m, MonadFlow m) => Id Person -> m [DriverRCAssociation]
-- findAllLinkedByDriverId driverId = do
--   now <- getCurrentTime
--   Esq.findAll $ do
--     association <- from $ table @DriverRCAssociationT
--     where_ $
--       association ^. DriverRCAssociationDriverId ==. val (toKey driverId)
--         &&. association ^. DriverRCAssociationAssociatedTill >. val (Just now)
--     orderBy [desc $ association ^. DriverRCAssociationAssociatedOn]
--     return association
findLinkedByRCIdAndDriverId :: (L.MonadFlow m, Log m) => Id Person -> Id VehicleRegistrationCertificate -> UTCTime -> m (Maybe DriverRCAssociation)
findLinkedByRCIdAndDriverId (Id driverId) (Id rcId) now = findOneWithKV [Se.And [Se.Is BeamDRCA.driverId $ Se.Eq driverId, Se.Is BeamDRCA.rcId $ Se.Eq rcId, Se.Is BeamDRCA.associatedTill $ Se.GreaterThan $ Just now]]

findLatestByRCIdAndDriverId :: (L.MonadFlow m, Log m) => Id VehicleRegistrationCertificate -> Id Person -> m (Maybe DriverRCAssociation)
findLatestByRCIdAndDriverId (Id rcId) (Id driverId) =
  findAllWithOptionsKV [Se.And [Se.Is BeamDRCA.rcId $ Se.Eq rcId, Se.Is BeamDRCA.driverId $ Se.Eq driverId]] (Se.Desc BeamDRCA.associatedTill) (Just 1) Nothing <&> listToMaybe

findAllLinkedByDriverId :: (L.MonadFlow m, Log m, MonadTime m) => Id Person -> m [DriverRCAssociation]
findAllLinkedByDriverId (Id driverId) = do
  now <- getCurrentTime
  findAllWithOptionsKV [Se.Is BeamDRCA.driverId $ Se.Eq driverId, Se.Is BeamDRCA.associatedTill $ Se.GreaterThan $ Just now] (Se.Desc BeamDRCA.associatedOn) Nothing Nothing
