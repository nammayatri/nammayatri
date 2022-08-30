{-# LANGUAGE TypeApplications #-}

module Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.DriverOnboarding.ClassOfVehicle
import Domain.Types.DriverOnboarding.VehicleRegistrationCertificate
import Domain.Types.Person (Person)
import Storage.Tabular.DriverOnboarding.VehicleRegistrationCertificate
import Storage.Tabular.Person ()

create :: VehicleRegistrationCertificate -> SqlDB ()
create = Esq.create

findById ::
  EsqDBFlow m r =>
  Id VehicleRegistrationCertificate ->
  m (Maybe VehicleRegistrationCertificate)
findById = Esq.findById

findLatestByPersonId ::
  Transactionable m =>
  Id Person ->
  m (Maybe VehicleRegistrationCertificate)
findLatestByPersonId personid = do
  certs <- findAll $ do
    vechileRegCert <- from $ table @VehicleRegistrationCertificateT
    where_ $ vechileRegCert ^. VehicleRegistrationCertificateDriverId ==. val (toKey personid)
    orderBy [asc $ vechileRegCert ^. VehicleRegistrationCertificateVersion]
    return vechileRegCert
  pure $ headMaybe certs
  where 
    headMaybe [] = Nothing
    headMaybe (x : _) = Just x

findActiveVehicleRC ::
  Transactionable m =>
  Text ->
  m (Maybe VehicleRegistrationCertificate)
findActiveVehicleRC certNumber = do
  findOne $ do
    vechileRegCert <- from $ table @VehicleRegistrationCertificateT
    where_ $
      vechileRegCert ^. VehicleRegistrationCertificateCertificateNumber ==. val certNumber
        &&. vechileRegCert ^. VehicleRegistrationCertificateActive ==. val True
    return vechileRegCert

makeRCInactive :: Id VehicleRegistrationCertificate -> UTCTime -> SqlDB ()
makeRCInactive id now = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ VehicleRegistrationCertificateActive =. val False,
        VehicleRegistrationCertificateUpdatedAt =. val now
      ]
    where_ $ tbl ^. VehicleRegistrationCertificateTId ==. val (toKey id)

updateRCDetails :: Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Maybe UTCTime -> Maybe UTCTime -> VerificationStatus -> Maybe ClassOfVehicle -> UTCTime -> SqlDB ()
updateRCDetails idfyRequestId permitStart permitValidity fitnessExpiry insuranceValidity verificationStatus cov now = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ VehicleRegistrationCertificateVehicleClass =. val cov,
        VehicleRegistrationCertificatePermitStart =. val permitStart,
        VehicleRegistrationCertificatePermitExpiry =. val permitValidity,
        VehicleRegistrationCertificateFitnessExpiry =. val fitnessExpiry,
        VehicleRegistrationCertificateInsuranceValidity =. val insuranceValidity,
        VehicleRegistrationCertificateVerificationStatus =. val verificationStatus,
        VehicleRegistrationCertificateUpdatedAt =. val now
      ]
    where_ $ tbl ^. VehicleRegistrationCertificateIdfyRequestId ==. val idfyRequestId

resetRCRequest :: Id Person -> Text -> Maybe Text -> UTCTime -> SqlDB ()
resetRCRequest driverId rcNumber idfyRequestId now = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ VehicleRegistrationCertificateCertificateNumber =. val rcNumber,
        VehicleRegistrationCertificateIdfyRequestId =. val idfyRequestId,
        VehicleRegistrationCertificateVerificationStatus =. val PENDING,
        VehicleRegistrationCertificateVehicleClass =. val Nothing,
        VehicleRegistrationCertificatePermitStart =. val Nothing,
        VehicleRegistrationCertificatePermitExpiry =. val Nothing,
        VehicleRegistrationCertificateFitnessExpiry =. val Nothing,
        VehicleRegistrationCertificateInsuranceValidity =. val Nothing,
        VehicleRegistrationCertificateUpdatedAt =. val now
      ]
    where_ $ tbl ^. VehicleRegistrationCertificateDriverId ==. val (toKey driverId)
