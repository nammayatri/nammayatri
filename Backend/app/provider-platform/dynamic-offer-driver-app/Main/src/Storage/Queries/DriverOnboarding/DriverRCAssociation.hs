{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

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

getActiveAssociationByDriver ::
  (Transactionable m, MonadFlow m) =>
  Id Person ->
  m (Maybe DriverRCAssociation)
getActiveAssociationByDriver driverId = do
  now <- getCurrentTime
  findOne $ do
    association <- from $ table @DriverRCAssociationT
    where_ $
      association ^. DriverRCAssociationDriverId ==. val (toKey driverId)
        &&. association ^. DriverRCAssociationAssociatedTill >. val (Just now)
    return association

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
  let rcHM = buildRcHM rcAssocs
   in mapMaybe (mapRCWithDriver rcHM) regCerts

mapRCWithDriver :: HashMap.HashMap Text DriverRCAssociation -> VehicleRegistrationCertificate -> Maybe (DriverRCAssociation, VehicleRegistrationCertificate)
mapRCWithDriver rcHM cert = do
  let rcId = cert.id.getId
  rcAssoc <- HashMap.lookup rcId rcHM
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

getActiveAssociationByRC ::
  (Transactionable m, MonadFlow m) =>
  Id VehicleRegistrationCertificate ->
  m (Maybe DriverRCAssociation)
getActiveAssociationByRC rcId = do
  now <- getCurrentTime
  findOne $ do
    association <- from $ table @DriverRCAssociationT
    where_ $
      association ^. DriverRCAssociationRcId ==. val (toKey rcId)
        &&. association ^. DriverRCAssociationAssociatedTill >. val (Just now)
    return association

endAssociation ::
  Id Person ->
  SqlDB ()
endAssociation driverId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverRCAssociationAssociatedTill =. val (Just now)
      ]
    where_ $
      tbl ^. DriverRCAssociationDriverId ==. val (toKey driverId)
        &&. tbl ^. DriverRCAssociationAssociatedTill >. val (Just now)

deleteByDriverId :: Id Person -> SqlDB ()
deleteByDriverId driverId =
  Esq.delete $ do
    associations <- from $ table @DriverRCAssociationT
    where_ $ associations ^. DriverRCAssociationDriverId ==. val (toKey driverId)
