module Domain.Action.Dashboard.Driver
  ( driverDocumentsInfo,
    listDrivers,
    driverActivity,
    enableDrivers,
    disableDrivers,
    driverLocation,
    driverInfo,
    deleteDriver,
    unlinkVehicle,
    updatePhoneNumber,
  )
where

import Beckn.External.Encryption (decrypt, encrypt)
import Beckn.External.Maps.Types (LatLong (..))
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.APISuccess (APISuccess (Success))
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Driver as Common
import Data.Coerce
import Data.List.NonEmpty (nonEmpty)
import Domain.Action.UI.DriverOnboarding.Status (ResponseStatus (..))
import qualified Domain.Action.UI.DriverOnboarding.Status as St
import qualified Domain.Types.DriverInformation as DrInfo
import Domain.Types.DriverOnboarding.DriverLicense
import Domain.Types.DriverOnboarding.DriverRCAssociation (DriverRCAssociation)
import qualified Domain.Types.DriverOnboarding.IdfyVerification as IV
import Domain.Types.DriverOnboarding.VehicleRegistrationCertificate
import qualified Domain.Types.Merchant as DM
import Domain.Types.Person
import qualified Domain.Types.Vehicle as DVeh
import Environment
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverLocation as QDriverLocation
import qualified Storage.Queries.DriverOnboarding.DriverLicense as QDriverLicense
import qualified Storage.Queries.DriverOnboarding.DriverRCAssociation as QRCAssociation
import qualified Storage.Queries.DriverOnboarding.IdfyVerification as QIV
import qualified Storage.Queries.DriverOnboarding.Image as QImage
import qualified Storage.Queries.DriverOnboarding.Status as QDocStatus
import qualified Storage.Queries.DriverQuote as QDriverQuote
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequestForDriver as QSearchReqForDriver
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Tools.Auth as Auth
import Tools.Error

-- FIXME: not tested yet because of no onboarding test data
driverDocumentsInfo :: ShortId DM.Merchant -> Flow Common.DriverDocumentsInfoRes
driverDocumentsInfo merchantShortId = do
  merchant <-
    CQM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  now <- getCurrentTime
  onboardingTryLimit <- asks (.driverOnboardingConfigs.onboardingTryLimit)
  drivers <- QDocStatus.fetchDriverDocsInfo merchant.id Nothing
  pure $ foldl' (func onboardingTryLimit now) Common.emptyInfo drivers
  where
    oneMonth :: NominalDiffTime
    oneMonth = 60 * 60 * 24 * 30
    func :: Int -> UTCTime -> Common.DriverDocumentsInfoRes -> QDocStatus.DriverDocsInfo -> Common.DriverDocumentsInfoRes
    func onboardingTryLimit now acc fd = do
      let mbLic = fd.license
          mbRegCert = fd.assocReg
          mbLicReq = fd.licenseVerificationReq
          mbVehRegReq = fd.regVerificationReq

          dlStatus = getLicenseStatus onboardingTryLimit fd.numLicenseImages mbLic mbLicReq
          rcStatus = getRegCertStatus onboardingTryLimit fd.numVehRegImages mbRegCert mbVehRegReq

          mbDlExpiration = (.licenseExpiry) <$> mbLic
          mbRcExpiration = getRcExpiration . snd <$> mbRegCert
          dlExpiresInMonth = expiresInMonth mbDlExpiration
          rcExpiresInMonth = expiresInMonth mbRcExpiration
          expiresInMonth mbExp = fromMaybe False $
            flip fmap mbExp $
              \exp_ -> let dif = diffUTCTime exp_ now in dif < oneMonth && dif > 0

      acc{Common.registered = acc.registered + 1,
          Common.verified = incrIf fd.driverInfo.verified acc.verified,
          Common.enabled = incrIf fd.driverInfo.enabled acc.enabled,
          Common.validDocuments = incrDocs (dlStatus == VALID) (rcStatus == VALID) acc.validDocuments,
          Common.invalidDocuments = incrDocs (dlStatus == INVALID) (rcStatus == INVALID) acc.invalidDocuments,
          Common.verificationPending = incrDocs (dlStatus == PENDING) (rcStatus == PENDING) acc.verificationPending,
          Common.verificationFailed = incrDocs (dlStatus == FAILED) (rcStatus == FAILED) acc.verificationFailed,
          Common.verificationLimitExceeded = incrDocs (dlStatus == LIMIT_EXCEED) (rcStatus == LIMIT_EXCEED) acc.verificationLimitExceeded,
          Common.docsExpiringInMonth = incrDocs dlExpiresInMonth rcExpiresInMonth acc.docsExpiringInMonth
         }

incrIf :: Num a => Bool -> a -> a
incrIf b = if b then (+ 1) else identity

incrDocs :: Bool -> Bool -> Common.DocumentsByStateInfo -> Common.DocumentsByStateInfo
incrDocs lic vehReg old =
  old{Common.driverLicense = incrIf lic old.driverLicense,
      Common.vehicleRegistrationCertificate = incrIf vehReg old.vehicleRegistrationCertificate
     }

getRcExpiration :: VehicleRegistrationCertificate -> UTCTime
getRcExpiration = (.fitnessExpiry)

getLicenseStatus :: Int -> Int -> Maybe DriverLicense -> Maybe IV.IdfyVerification -> St.ResponseStatus
getLicenseStatus onboardingTryLimit currentTries mbLicense mbLicReq =
  case mbLicense of
    Just driverLicense -> St.mapStatus driverLicense.verificationStatus
    Nothing -> St.verificationStatus onboardingTryLimit currentTries mbLicReq

getRegCertStatus :: Int -> Int -> Maybe (DriverRCAssociation, VehicleRegistrationCertificate) -> Maybe IV.IdfyVerification -> St.ResponseStatus
getRegCertStatus onboardingTryLimit currentTries mbRegCert mbVehRegReq =
  case mbRegCert of
    Just (_assoc, vehicleRC) -> St.mapStatus vehicleRC.verificationStatus
    Nothing -> St.verificationStatus onboardingTryLimit currentTries mbVehRegReq

---------

-- FIXME remove this, all entities should be limited on db level
limitOffset :: Maybe Int -> Maybe Int -> [a] -> [a]
limitOffset mbLimit mbOffset =
  maybe identity take mbLimit . maybe identity drop mbOffset

---------------------------------------------------------------------
listDrivers :: ShortId DM.Merchant -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Text -> Flow Common.DriverListRes
listDrivers merchantShortId mbLimit mbOffset mbVerified mbEnabled mbSearchPhone = do
  merchant <-
    CQM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  let limit = min maxLimit . fromMaybe defaultLimit $ mbLimit
      offset = fromMaybe 0 mbOffset
  driversWithInfo <- QPerson.findAllDriversWithInfoAndVehicle merchant.id limit offset mbVerified mbEnabled mbSearchPhone
  items <- mapM buildDriverListItem driversWithInfo
  pure $ Common.DriverListRes (length items) items
  where
    maxLimit = 20
    defaultLimit = 10

buildDriverListItem :: EncFlow m r => (Person, DrInfo.DriverInformation, Maybe DVeh.Vehicle) -> m Common.DriverListItem
buildDriverListItem (person, driverInformation, mbVehicle) = do
  phoneNo <- mapM decrypt person.mobileNumber
  pure $
    Common.DriverListItem
      { driverId = cast @Person @Common.Driver person.id,
        firstName = person.firstName,
        middleName = person.middleName,
        lastName = person.lastName,
        vehicleNo = mbVehicle <&> (.registrationNo),
        phoneNo,
        enabled = driverInformation.enabled,
        verified = driverInformation.verified,
        onRide = driverInformation.onRide,
        active = driverInformation.active
      }

---------------------------------------------------------------------
driverActivity :: ShortId DM.Merchant -> Flow Common.DriverActivityRes
driverActivity merchantShortId = do
  merchant <-
    CQM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  foldl' func Common.emptyDriverActivityRes <$> QPerson.findAllDrivers merchant.id
  where
    func :: Common.DriverActivityRes -> QPerson.FullDriver -> Common.DriverActivityRes
    func acc x =
      if x.info.active
        then acc {Common.activeDriversInApp = acc.activeDriversInApp + 1}
        else acc {Common.inactiveDrivers = acc.inactiveDrivers + 1}

---------------------------------------------------------------------
enableDrivers :: ShortId DM.Merchant -> Common.DriverIds -> Flow Common.EnableDriversRes
enableDrivers merchantShortId req = do
  merchant <-
    CQM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  let enable = True
  updatedDrivers <- QDriverInfo.updateEnabledStateReturningIds merchant.id (coerce req.driverIds) enable
  let driversNotFound = filter (not . (`elem` coerce @[Id Driver] @[Id Common.Driver] updatedDrivers)) req.driverIds
  let numDriversEnabled = length updatedDrivers
  pure $
    Common.EnableDriversRes
      { numDriversEnabled,
        driversEnabled = coerce updatedDrivers,
        message = mconcat [show numDriversEnabled, " drivers enabled, following drivers not found: ", show $ coerce @_ @[Text] driversNotFound]
      }

---------------------------------------------------------------------
disableDrivers :: ShortId DM.Merchant -> Common.DriverIds -> Flow Common.DisableDriversRes
disableDrivers merchantShortId req = do
  merchant <-
    CQM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  let enable = False
  updatedDrivers <- QDriverInfo.updateEnabledStateReturningIds merchant.id (coerce req.driverIds) enable
  let driversNotFound = filter (not . (`elem` coerce @[Id Driver] @[Id Common.Driver] updatedDrivers)) req.driverIds
  let numDriversDisabled = length updatedDrivers
  pure $
    Common.DisableDriversRes
      { numDriversDisabled,
        driversDisabled = coerce updatedDrivers,
        message = mconcat [show numDriversDisabled, " drivers disabled, following drivers not found: ", show $ coerce @_ @[Text] driversNotFound]
      }

---------------------------------------------------------------------
driverLocation :: ShortId DM.Merchant -> Maybe Int -> Maybe Int -> Common.DriverIds -> Flow Common.DriverLocationRes
driverLocation merchantShortId mbLimit mbOffset req = do
  merchant <-
    CQM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  let driverIds = coerce req.driverIds
  allDrivers <- QPerson.findAllDriversByIdsFirstNameAsc merchant.id driverIds
  let driversNotFound =
        filter (not . (`elem` map ((.id) . (.person)) allDrivers)) driverIds
      limitedDrivers = limitOffset mbLimit mbOffset allDrivers
  resultList <- mapM buildDriverLocationListItem limitedDrivers
  pure $ Common.DriverLocationRes (nonEmpty $ coerce driversNotFound) resultList

buildDriverLocationListItem :: EncFlow m r => QPerson.FullDriver -> m Common.DriverLocationItem
buildDriverLocationListItem f = do
  let p = f.person
      v = f.vehicle
  phoneNo <- maybe (pure "") decrypt p.mobileNumber
  pure
    Common.DriverLocationItem
      { driverId = cast p.id,
        firstName = p.firstName,
        middleName = p.middleName,
        lastName = p.lastName,
        vehicleNo = v.registrationNo,
        phoneNo,
        active = f.info.active,
        onRide = f.info.onRide,
        location = LatLong f.location.lat f.location.lon,
        lastLocationTimestamp = f.location.coordinatesCalculatedAt
      }

---------------------------------------------------------------------
-- FIXME Do we need to include mobileCountryCode into query params?
mobileIndianCode :: Text
mobileIndianCode = "+91"

driverInfo :: ShortId DM.Merchant -> Maybe Text -> Maybe Text -> Flow Common.DriverInfoRes
driverInfo merchantShortId mbMobileNumber mbVehicleNumber = do
  merchant <-
    CQM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  driverDocsInfo <- case (mbMobileNumber, mbVehicleNumber) of
    (Just mobileNumber, Nothing) ->
      QDocStatus.fetchFullDriverInfoWithDocsByMobileNumber merchant.id mobileNumber mobileIndianCode
        >>= fromMaybeM (PersonDoesNotExist $ mobileIndianCode <> mobileNumber)
    (Nothing, Just vehicleNumber) ->
      QDocStatus.fetchFullDriverInfoWithDocsByVehNumber merchant.id vehicleNumber
        >>= fromMaybeM (VehicleDoesNotExist vehicleNumber)
    _ -> throwError $ InvalidRequest "Exactly one of query parameters \"mobileNumber\", \"vehicleNumber\" is required"
  buildDriverInfoRes driverDocsInfo
  where
    buildDriverInfoRes :: (EncFlow m r) => QDocStatus.FullDriverWithDocs -> m Common.DriverInfoRes
    buildDriverInfoRes driver@QDocStatus.FullDriverWithDocs {..} = do
      mobileNumber <- traverse decrypt person.mobileNumber
      dlNumber <- traverse decrypt $ license <&> (.licenseNumber)
      let vehicleDetails = mkVehicleAPIEntity driver <$> (vehicle <&> (.registrationNo))
      pure
        Common.DriverInfoRes
          { driverId = cast @Person @Common.Driver person.id,
            firstName = person.firstName, -- license.driverName ?
            middleName = person.middleName,
            lastName = person.lastName,
            dlNumber,
            dateOfBirth = join $ license <&> (.driverDob),
            numberOfRides = fromMaybe 0 ridesCount,
            mobileNumber,
            enabled = info.enabled,
            verified = info.verified,
            vehicleDetails
          }

    mkVehicleAPIEntity :: QDocStatus.FullDriverWithDocs -> Text -> Common.VehicleAPIEntity
    mkVehicleAPIEntity QDocStatus.FullDriverWithDocs {..} vehicleNumber = do
      let mbAssociation = fst <$> registration
      let mbCertificate = snd <$> registration
      Common.VehicleAPIEntity
        { vehicleNumber,
          dateOfReg = mbAssociation <&> (.associatedOn),
          vehicleClass = join $ mbCertificate <&> (.vehicleClass)
        }

---------------------------------------------------------------------
deleteDriver :: ShortId DM.Merchant -> Id Common.Driver -> Flow APISuccess
deleteDriver merchantShortId reqDriverId = do
  merchant <-
    CQM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  let driverId = cast @Common.Driver @Driver reqDriverId
  let personId = cast @Common.Driver @Person reqDriverId
  driver <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  merchantId <- driver.merchantId & fromMaybeM (PersonFieldNotPresent "merchant_id")
  unless (merchant.id == merchantId) $ throwError (PersonDoesNotExist personId.getId)

  unless (driver.role == DRIVER) $ throwError Unauthorized

  ride <- QRide.findOneByDriverId personId
  unless (isNothing ride) $
    throwError $ InvalidRequest "Unable to delete driver, which have at least one ride"

  dl <- QDriverLicense.findByDriverId personId
  unless (isNothing dl) $
    throwError $ InvalidRequest "Unable to delete driver, which have driver license"

  rcAssociation <- QRCAssociation.findOneByDriverId personId
  unless (isNothing rcAssociation) $
    throwError $ InvalidRequest "Unable to delete driver, which have registration certificate associated"

  driverInformation <- QDriverInfo.findById driverId >>= fromMaybeM DriverInfoNotFound
  when driverInformation.enabled $
    throwError $ InvalidRequest "Driver should be disabled before deletion"

  -- this function uses tokens from db, so should be called before transaction
  Auth.clearDriverSession personId
  Esq.runTransaction $ do
    QIV.deleteByPersonId personId
    QImage.deleteByPersonId personId
    QDriverQuote.deleteByDriverId personId
    QSearchReqForDriver.deleteByDriverId personId
    QDriverInfo.deleteById driverId
    QDriverStats.deleteById driverId
    QDriverLocation.deleteById personId
    QR.deleteByPersonId personId
    QVehicle.deleteById personId
    QPerson.deleteById personId
  logTagInfo "dashboard -> deleteDriver : " (show driverId)
  return Success

---------------------------------------------------------------------
unlinkVehicle :: ShortId DM.Merchant -> Id Common.Driver -> Flow APISuccess
unlinkVehicle merchantShortId reqDriverId = do
  merchant <-
    CQM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)

  let personId = cast @Common.Driver @Person reqDriverId
  driver <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  merchantId <- driver.merchantId & fromMaybeM (PersonFieldNotPresent "merchant_id")
  unless (merchant.id == merchantId) $ throwError (PersonDoesNotExist personId.getId)

  Esq.runTransaction $ do
    QVehicle.deleteById personId
    QRCAssociation.endAssociation personId
  logTagInfo "dashboard -> unlinkVehicle : " (show personId)
  pure Success

---------------------------------------------------------------------
-- FIXME Do we need to include mobileCountryCode into request?
updatePhoneNumber :: ShortId DM.Merchant -> Id Common.Driver -> Common.UpdatePhoneNumberReq -> Flow APISuccess
updatePhoneNumber merchantShortId reqDriverId req = do
  merchant <-
    CQM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)

  let newPhoneNumber = req.newPhoneNumber
  let personId = cast @Common.Driver @Person reqDriverId
  driver <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  merchantId <- driver.merchantId & fromMaybeM (PersonFieldNotPresent "merchant_id")
  unless (merchant.id == merchantId) $ throwError (PersonDoesNotExist personId.getId)

  mbLinkedPerson <- QPerson.findByMobileNumber mobileIndianCode newPhoneNumber
  whenJust mbLinkedPerson $ \_ -> throwError $ InvalidRequest "Person with this mobile number already exists"

  encNewPhoneNumber <- encrypt newPhoneNumber
  let updDriver =
        driver
          { mobileCountryCode = Just mobileIndianCode,
            mobileNumber = Just encNewPhoneNumber
          }
  -- this function uses tokens from db, so should be called before transaction
  Auth.clearDriverSession personId
  Esq.runTransaction $ do
    QPerson.updateMobileNumberAndCode updDriver
    QR.deleteByPersonId personId
  logTagInfo "dashboard -> updatePhoneNumber : " (show personId)
  pure Success
