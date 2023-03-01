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

import Domain.Types.DriverOnboarding.DriverRCAssociation
import Domain.Types.DriverOnboarding.VehicleRegistrationCertificate
import Domain.Types.Person (Person)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.DriverOnboarding.DriverRCAssociation
import Storage.Tabular.DriverOnboarding.VehicleRegistrationCertificate

create :: DriverRCAssociation -> SqlDB m ()
create = Esq.create

findById ::
  forall m ma.
  Transactionable ma m =>
  Proxy ma ->
  Id DriverRCAssociation ->
  m (Maybe DriverRCAssociation)
findById _ = Esq.findById @m @ma

getActiveAssociationByDriver ::
  forall m ma.
  (Transactionable ma m, MonadFlow m) =>
  Id Person ->
  Proxy ma ->
  m (Maybe DriverRCAssociation)
getActiveAssociationByDriver driverId _ = do
  now <- getCurrentTime
  findOne @m @ma $ do
    association <- from $ table @DriverRCAssociationT
    where_ $
      association ^. DriverRCAssociationDriverId ==. val (toKey driverId)
        &&. association ^. DriverRCAssociationAssociatedTill >. val (Just now)
    return association

findAllByDriverId ::
  forall m ma.
  Transactionable ma m =>
  Id Person ->
  Proxy ma ->
  m [(DriverRCAssociation, VehicleRegistrationCertificate)]
findAllByDriverId driverId _ = do
  findAll @m @ma $ do
    rcAssoc :& regCert <-
      from $
        table @DriverRCAssociationT
          `Esq.innerJoin` table @VehicleRegistrationCertificateT
            `Esq.on` ( \(rcAssoc :& regCert) ->
                         rcAssoc ^. DriverRCAssociationRcId ==. regCert ^. VehicleRegistrationCertificateTId
                     )
    where_ $
      rcAssoc ^. DriverRCAssociationDriverId ==. val (toKey driverId)
    orderBy [desc $ rcAssoc ^. DriverRCAssociationAssociatedOn]
    return (rcAssoc, regCert)

getActiveAssociationByRC ::
  forall m ma.
  (Transactionable ma m, MonadFlow m) =>
  Id VehicleRegistrationCertificate ->
  Proxy ma ->
  m (Maybe DriverRCAssociation)
getActiveAssociationByRC rcId _ = do
  now <- getCurrentTime
  findOne @m @ma $ do
    association <- from $ table @DriverRCAssociationT
    where_ $
      association ^. DriverRCAssociationRcId ==. val (toKey rcId)
        &&. association ^. DriverRCAssociationAssociatedTill >. val (Just now)
    return association

endAssociation ::
  Id Person ->
  SqlDB m ()
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

deleteByDriverId :: Id Person -> SqlDB m ()
deleteByDriverId driverId =
  Esq.delete $ do
    associations <- from $ table @DriverRCAssociationT
    where_ $ associations ^. DriverRCAssociationDriverId ==. val (toKey driverId)
