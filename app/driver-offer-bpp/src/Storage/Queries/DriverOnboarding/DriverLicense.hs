{-# LANGUAGE TypeApplications #-}

module Storage.Queries.DriverOnboarding.DriverLicense where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.DriverOnboarding.ClassOfVehicle (ClassOfVehicle (..), VerificationStatus (..))
import Domain.Types.DriverOnboarding.DriverLicense
import Domain.Types.Person (Person)
import Storage.Tabular.DriverOnboarding.DriverLicense
import Storage.Tabular.Person ()

create :: DriverLicense -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id DriverLicense ->
  m (Maybe DriverLicense)
findById = Esq.findById

findByDriverId ::
  Transactionable m =>
  Id Person ->
  m (Maybe DriverLicense)
findByDriverId personid = do
  findOne $ do
    driverLicense <- from $ table @DriverLicenseT
    where_ $ driverLicense ^. DriverLicenseDriverId ==. val (toKey personid)
    return driverLicense

updateImagePath ::
  Id Person ->
  Text ->
  SqlDB ()
updateImagePath personid path = do
  Esq.update $ \tbl -> do
    set
      tbl
      [DriverLicenseImageS3Path =. val path]
    where_ $ tbl ^. DriverLicenseDriverId ==. val (toKey personid)

updateDLDetails :: Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> VerificationStatus -> [ClassOfVehicle] -> UTCTime -> SqlDB () -- [ClassOfVehicle] changed to Maybe [ClassOfVehicle]
updateDLDetails idfyRequestId start expiry verificationStatus cov now = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverLicenseClassOfVehicles =. val (PostgresList cov),
        DriverLicenseLicenseStart =. val start,
        DriverLicenseLicenseExpiry =. val expiry,
        DriverLicenseVerificationStatus =. val verificationStatus,
        DriverLicenseUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverLicenseIdfyRequestId ==. val idfyRequestId

resetDLRequest :: Id Person -> Text -> Maybe UTCTime -> Maybe Text -> UTCTime -> SqlDB ()
resetDLRequest driverId dlNumber dob idfyRequestId now = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ DriverLicenseLicenseNumber =. val dlNumber,
        DriverLicenseDriverDob =. val dob,
        DriverLicenseIdfyRequestId =. val idfyRequestId,
        DriverLicenseClassOfVehicles =. val (PostgresList []),
        DriverLicenseLicenseStart =. val Nothing,
        DriverLicenseLicenseExpiry =. val Nothing,
        DriverLicenseVerificationStatus =. val PENDING,
        DriverLicenseUpdatedAt =. val now
      ]
    where_ $ tbl ^. DriverLicenseDriverId ==. val (toKey driverId)
