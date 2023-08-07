{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverOnboarding.DriverRCAssociation where

import qualified Data.HashMap.Strict as HashMap
import Domain.Types.DriverOnboarding.DriverRCAssociation
import Domain.Types.DriverOnboarding.VehicleRegistrationCertificate
import Domain.Types.Person (Person)
import Kernel.Prelude hiding (on)
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.DriverOnboarding.DriverRCAssociation
import Storage.Tabular.DriverOnboarding.VehicleRegistrationCertificate

create :: DriverRCAssociation -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id DriverRCAssociation ->
  m (Maybe DriverRCAssociation)
findById = Esq.findById

findAllByDriverId ::
  Transactionable m =>
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

getRegCerts ::
  Transactionable m =>
  [DriverRCAssociation] ->
  m [VehicleRegistrationCertificate]
getRegCerts rcAssocs = do
  Esq.findAll $ do
    regCerts <- from $ table @VehicleRegistrationCertificateT
    where_ $
      regCerts ^. VehicleRegistrationCertificateTId `in_` valList rcAssocsKeys
    return regCerts
  where
    rcAssocsKeys = toKey . cast <$> fetchRcIdFromAssocs rcAssocs

fetchRcIdFromAssocs :: [DriverRCAssociation] -> [Id VehicleRegistrationCertificate]
fetchRcIdFromAssocs = map (.rcId)

getRcAssocs ::
  Transactionable m =>
  Id Person ->
  m [DriverRCAssociation]
getRcAssocs driverId = do
  Esq.findAll $ do
    rcAssoc <- from $ table @DriverRCAssociationT
    where_ $
      rcAssoc ^. DriverRCAssociationDriverId ==. val (toKey driverId)
    orderBy [desc $ rcAssoc ^. DriverRCAssociationAssociatedOn]
    return rcAssoc

endAssociationForRC ::
  Id Person ->
  Id VehicleRegistrationCertificate ->
  SqlDB ()
endAssociationForRC driverId rcId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverRCAssociationAssociatedTill =. val (Just now)
      ]
    where_ $
      tbl ^. DriverRCAssociationDriverId ==. val (toKey driverId)
        &&. tbl ^. DriverRCAssociationRcId ==. val (toKey rcId)
        &&. tbl ^. DriverRCAssociationAssociatedTill >. val (Just now)

deleteByDriverId :: Id Person -> SqlDB ()
deleteByDriverId driverId =
  Esq.delete $ do
    associations <- from $ table @DriverRCAssociationT
    where_ $ associations ^. DriverRCAssociationDriverId ==. val (toKey driverId)

findActiveAssociationByRC :: (Transactionable m) => Id VehicleRegistrationCertificate -> m (Maybe DriverRCAssociation)
findActiveAssociationByRC rcId =
  Esq.findOne $ do
    rcAssoc <- from $ table @DriverRCAssociationT
    where_ $
      rcAssoc ^. DriverRCAssociationRcId ==. val (toKey rcId)
        &&. rcAssoc ^. DriverRCAssociationIsRcActive ==. val True
    return rcAssoc

findActiveAssociationByDriver ::
  Transactionable m =>
  Id Person ->
  m (Maybe DriverRCAssociation)
findActiveAssociationByDriver driverId = do
  findOne $ do
    association <- from $ table @DriverRCAssociationT
    where_ $
      association ^. DriverRCAssociationDriverId ==. val (toKey driverId)
        &&. association ^. DriverRCAssociationIsRcActive ==. val True
    return association

deactivateRCForDriver :: Id Person -> Id VehicleRegistrationCertificate -> SqlDB ()
deactivateRCForDriver driverId rcId = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverRCAssociationIsRcActive =. val False
      ]
    where_ $
      tbl ^. DriverRCAssociationDriverId ==. val (toKey driverId)
        &&. tbl ^. DriverRCAssociationRcId ==. val (toKey rcId)

activateRCForDriver :: Id Person -> Id VehicleRegistrationCertificate -> UTCTime -> SqlDB ()
activateRCForDriver driverId rcId now = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverRCAssociationIsRcActive =. val True
      ]
    where_ $
      tbl ^. DriverRCAssociationDriverId ==. val (toKey driverId)
        &&. tbl ^. DriverRCAssociationRcId ==. val (toKey rcId)
        &&. tbl ^. DriverRCAssociationAssociatedTill >. val (Just now)

findLinkedByRCIdAndDriverId :: Transactionable m => Id Person -> Id VehicleRegistrationCertificate -> UTCTime -> m (Maybe DriverRCAssociation)
findLinkedByRCIdAndDriverId driverId rcId now = do
  Esq.findOne $ do
    res <- from $ table @DriverRCAssociationT
    where_ $
      res ^. DriverRCAssociationDriverId ==. val (toKey driverId)
        &&. res ^. DriverRCAssociationRcId ==. val (toKey rcId)
        &&. res ^. DriverRCAssociationAssociatedTill >. val (Just now)
    return res

findLatestByRCIdAndDriverId :: Transactionable m => Id VehicleRegistrationCertificate -> Id Person -> m (Maybe DriverRCAssociation)
findLatestByRCIdAndDriverId rcId driverId =
  Esq.findOne $ do
    rcAssoc <- from $ table @DriverRCAssociationT
    where_ $
      rcAssoc ^. DriverRCAssociationRcId ==. val (toKey rcId)
        &&. rcAssoc ^. DriverRCAssociationDriverId ==. val (toKey driverId)
    orderBy [desc $ rcAssoc ^. DriverRCAssociationAssociatedTill]
    limit 1
    return rcAssoc

findAllLinkedByDriverId :: (Transactionable m, MonadFlow m) => Id Person -> m [DriverRCAssociation]
findAllLinkedByDriverId driverId = do
  now <- getCurrentTime
  Esq.findAll $ do
    association <- from $ table @DriverRCAssociationT
    where_ $
      association ^. DriverRCAssociationDriverId ==. val (toKey driverId)
        &&. association ^. DriverRCAssociationAssociatedTill >. val (Just now)
    orderBy [desc $ association ^. DriverRCAssociationAssociatedOn]
    return association
