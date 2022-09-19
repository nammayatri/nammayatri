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

getActiveRCByDriver ::
  Transactionable m =>
  Id Person ->
  m (Maybe (DriverRCAssociation, VehicleRegistrationCertificate))
getActiveRCByDriver driverId = do
  findOne $ do
    (association :& regCert) <-
      from $
        table @DriverRCAssociationT `Esq.innerJoin` table @VehicleRegistrationCertificateT
          `Esq.on` (\(a :& cert) -> a ^. DriverRCAssociationRcId ==. cert ^. VehicleRegistrationCertificateTId)
    where_ $
      association ^. DriverRCAssociationDriverId ==. val (toKey driverId)
        &&. association ^. DriverRCAssociationAssociatedTill ==. val Nothing
    return (association, regCert)

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
