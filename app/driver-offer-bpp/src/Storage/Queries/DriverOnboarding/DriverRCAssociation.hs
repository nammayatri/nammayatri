{-# LANGUAGE TypeApplications #-}

module Storage.Queries.DriverOnboarding.DriverRCAssociation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.DriverOnboarding.DriverRCAssociation
import Domain.Types.DriverOnboarding.VehicleRegistrationCertificate
import Domain.Types.Person (Person)
import Storage.Tabular.DriverOnboarding.DriverRCAssociation

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

findOneByDriverId ::
  (Transactionable m, MonadFlow m) =>
  Id Person ->
  m (Maybe DriverRCAssociation)
findOneByDriverId driverId = do
  findOne $ do
    association <- from $ table @DriverRCAssociationT
    where_ $
      association ^. DriverRCAssociationDriverId ==. val (toKey driverId)
    limit 1
    return association

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