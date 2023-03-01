{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Driver
  ( driverDocumentsInfo,
    listDrivers,
    driverActivity,
    enableDriver,
    disableDriver,
    blockDriver,
    unblockDriver,
    driverLocation,
    driverInfo,
    deleteDriver,
    unlinkVehicle,
    unlinkDL,
    endRCAssociation,
    updatePhoneNumber,
    addVehicle,
    updateDriverName,
  )
where

import Control.Applicative ((<|>))
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver as Common
import Data.Coerce
import Data.List.NonEmpty (nonEmpty)
import Domain.Action.UI.DriverOnboarding.Status (ResponseStatus (..))
import qualified Domain.Action.UI.DriverOnboarding.Status as St
import qualified Domain.Types.DriverInformation as DrInfo
import Domain.Types.DriverOnboarding.DriverLicense
import Domain.Types.DriverOnboarding.DriverRCAssociation (DriverRCAssociation (..))
import qualified Domain.Types.DriverOnboarding.IdfyVerification as IV
import Domain.Types.DriverOnboarding.Image (Image)
import Domain.Types.DriverOnboarding.VehicleRegistrationCertificate
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Vehicle as DVeh
import Environment
import Kernel.External.Encryption (decrypt, encrypt, getDbHash)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation (runRequestValidation)
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.DriverInformation as CQDriverInfo
import qualified Storage.Queries.Driver.DriverFlowStatus as QDriverFlowStatus
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
  merchant <- findMerchantByShortId merchantShortId
  now <- getCurrentTime
  onboardingTryLimit <- asks (.driverOnboardingConfigs.onboardingTryLimit)
  drivers <- Esq.runInReplica $ QDocStatus.fetchDriverDocsInfo merchant.id Nothing (Proxy @Flow)
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
          Common.blocked = incrIf (not fd.driverInfo.blocked) acc.blocked,
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
listDrivers :: ShortId DM.Merchant -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Flow Common.DriverListRes
listDrivers merchantShortId mbLimit mbOffset mbVerified mbEnabled mbBlocked mbSearchPhone = do
  merchant <- findMerchantByShortId merchantShortId
  let limit = min maxLimit . fromMaybe defaultLimit $ mbLimit
      offset = fromMaybe 0 mbOffset
  mbSearchPhoneDBHash <- getDbHash `traverse` mbSearchPhone
  driversWithInfo <- Esq.runInReplica $ QPerson.findAllDriversWithInfoAndVehicle merchant.id limit offset mbVerified mbEnabled mbBlocked mbSearchPhoneDBHash (Proxy @Flow)
  items <- mapM buildDriverListItem driversWithInfo
  let count = length items
  -- should we consider filters in totalCount, e.g. count all enabled drivers?
  totalCount <- Esq.runInReplica $ QPerson.countDrivers merchant.id (Proxy @Flow)
  let summary = Common.Summary {totalCount, count}
  pure Common.DriverListRes {totalItems = count, summary, drivers = items}
  where
    maxLimit = 20
    defaultLimit = 10

buildDriverListItem :: EncFlow m r => (DP.Person, DrInfo.DriverInformation, Maybe DVeh.Vehicle) -> m Common.DriverListItem
buildDriverListItem (person, driverInformation, mbVehicle) = do
  phoneNo <- mapM decrypt person.mobileNumber
  pure $
    Common.DriverListItem
      { driverId = cast @DP.Person @Common.Driver person.id,
        firstName = person.firstName,
        middleName = person.middleName,
        lastName = person.lastName,
        vehicleNo = mbVehicle <&> (.registrationNo),
        phoneNo,
        enabled = driverInformation.enabled,
        blocked = driverInformation.blocked,
        verified = driverInformation.verified,
        onRide = driverInformation.onRide,
        active = driverInformation.active
      }

---------------------------------------------------------------------
driverActivity :: ShortId DM.Merchant -> Flow Common.DriverActivityRes
driverActivity merchantShortId = do
  merchant <- findMerchantByShortId merchantShortId
  Common.mkDriverActivityRes <$> Esq.runInReplica (CQDriverInfo.countDrivers merchant.id (Proxy @Flow))

---------------------------------------------------------------------
enableDriver :: ShortId DM.Merchant -> Id Common.Driver -> Flow APISuccess
enableDriver merchantShortId reqDriverId = do
  merchant <- findMerchantByShortId merchantShortId

  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById (Proxy @Flow) personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  unless (merchant.id == driver.merchantId) $ throwError (PersonDoesNotExist personId.getId)

  _vehicle <- QVehicle.findById (Proxy @Flow) personId >>= fromMaybeM (VehicleDoesNotExist personId.getId)
  CQDriverInfo.updateEnabledState driverId True
  logTagInfo "dashboard -> enableDriver : " (show personId)
  pure Success

---------------------------------------------------------------------
disableDriver :: ShortId DM.Merchant -> Id Common.Driver -> Flow APISuccess
disableDriver merchantShortId reqDriverId = do
  merchant <- findMerchantByShortId merchantShortId

  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById (Proxy @Flow) personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  unless (merchant.id == driver.merchantId) $ throwError (PersonDoesNotExist personId.getId)

  CQDriverInfo.updateEnabledState driverId False
  logTagInfo "dashboard -> disableDriver : " (show personId)
  pure Success

---------------------------------------------------------------------
---------------------------------------------------------------------
blockDriver :: ShortId DM.Merchant -> Id Common.Driver -> Flow APISuccess
blockDriver merchantShortId reqDriverId = do
  merchant <- findMerchantByShortId merchantShortId

  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    Esq.runInReplica (QPerson.findById (Proxy @Flow) personId)
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId) $ throwError (PersonDoesNotExist personId.getId)

  CQDriverInfo.updateBlockedState driverId True
  logTagInfo "dashboard -> blockDriver : " (show personId)
  pure Success

---------------------------------------------------------------------
unblockDriver :: ShortId DM.Merchant -> Id Common.Driver -> Flow APISuccess
unblockDriver merchantShortId reqDriverId = do
  merchant <- findMerchantByShortId merchantShortId

  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    Esq.runInReplica (QPerson.findById (Proxy @Flow) personId)
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId) $ throwError (PersonDoesNotExist personId.getId)

  CQDriverInfo.updateBlockedState driverId False
  logTagInfo "dashboard -> unblockDriver : " (show personId)
  pure Success

driverLocation :: ShortId DM.Merchant -> Maybe Int -> Maybe Int -> Common.DriverIds -> Flow Common.DriverLocationRes
driverLocation merchantShortId mbLimit mbOffset req = do
  merchant <- findMerchantByShortId merchantShortId
  let driverIds = coerce req.driverIds
  allDrivers <- Esq.runInReplica $ QPerson.findAllDriversByIdsFirstNameAsc merchant.id driverIds (Proxy @Flow)
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
mobileIndianCode :: Text
mobileIndianCode = "+91"

driverInfo :: ShortId DM.Merchant -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Flow Common.DriverInfoRes
driverInfo merchantShortId mbMobileNumber mbMobileCountryCode mbVehicleNumber mbDlNumber mbRcNumber = do
  when (isJust mbMobileCountryCode && isNothing mbMobileNumber) $
    throwError $ InvalidRequest "\"mobileCountryCode\" can be used only with \"mobileNumber\""
  merchant <- findMerchantByShortId merchantShortId
  driverWithRidesCount <- case (mbMobileNumber, mbVehicleNumber, mbDlNumber, mbRcNumber) of
    (Just mobileNumber, Nothing, Nothing, Nothing) -> do
      mobileNumberDbHash <- getDbHash mobileNumber
      let mobileCountryCode = fromMaybe mobileIndianCode mbMobileCountryCode
      Esq.runInReplica $
        QPerson.fetchDriverInfoWithRidesCount merchant.id (Just (mobileNumberDbHash, mobileCountryCode)) Nothing Nothing Nothing (Proxy @Flow)
          >>= fromMaybeM (PersonDoesNotExist $ mobileCountryCode <> mobileNumber)
    (Nothing, Just vehicleNumber, Nothing, Nothing) -> do
      Esq.runInReplica $
        QPerson.fetchDriverInfoWithRidesCount merchant.id Nothing (Just vehicleNumber) Nothing Nothing (Proxy @Flow)
          >>= fromMaybeM (VehicleDoesNotExist vehicleNumber)
    (Nothing, Nothing, Just driverLicenseNumber, Nothing) -> do
      dlNumberHash <- getDbHash driverLicenseNumber
      Esq.runInReplica $
        QPerson.fetchDriverInfoWithRidesCount merchant.id Nothing Nothing (Just dlNumberHash) Nothing (Proxy @Flow)
          >>= fromMaybeM (InvalidRequest "License does not exist.")
    (Nothing, Nothing, Nothing, Just rcNumber) -> do
      rcNumberHash <- getDbHash rcNumber
      Esq.runInReplica $
        QPerson.fetchDriverInfoWithRidesCount merchant.id Nothing Nothing Nothing (Just rcNumberHash) (Proxy @Flow)
          >>= fromMaybeM (InvalidRequest "Registration certificate does not exist.")
    _ -> throwError $ InvalidRequest "Exactly one of query parameters \"mobileNumber\", \"vehicleNumber\", \"dlNumber\", \"rcNumber\" is required"
  let driverId = driverWithRidesCount.person.id
  mbDriverLicense <- Esq.runInReplica $ QDriverLicense.findByDriverId driverId (Proxy @Flow)
  rcAssociationHistory <- Esq.runInReplica $ QRCAssociation.findAllByDriverId driverId (Proxy @Flow)
  buildDriverInfoRes driverWithRidesCount mbDriverLicense rcAssociationHistory

buildDriverInfoRes ::
  EncFlow m r =>
  QPerson.DriverWithRidesCount ->
  Maybe DriverLicense ->
  [(DriverRCAssociation, VehicleRegistrationCertificate)] ->
  m Common.DriverInfoRes
buildDriverInfoRes QPerson.DriverWithRidesCount {..} mbDriverLicense rcAssociationHistory = do
  mobileNumber <- traverse decrypt person.mobileNumber
  driverLicenseDetails <- traverse buildDriverLicenseAPIEntity mbDriverLicense
  vehicleRegistrationDetails <- traverse buildRCAssociationAPIEntity rcAssociationHistory
  pure
    Common.DriverInfoRes
      { driverId = cast @DP.Person @Common.Driver person.id,
        firstName = person.firstName,
        middleName = person.middleName,
        lastName = person.lastName,
        numberOfRides = fromMaybe 0 ridesCount,
        mobileNumber,
        mobileCountryCode = person.mobileCountryCode,
        enabled = info.enabled,
        blocked = info.blocked,
        verified = info.verified,
        vehicleNumber = vehicle <&> (.registrationNo),
        driverLicenseDetails,
        vehicleRegistrationDetails
      }

buildDriverLicenseAPIEntity :: EncFlow m r => DriverLicense -> m Common.DriverLicenseAPIEntity
buildDriverLicenseAPIEntity DriverLicense {..} = do
  licenseNumber' <- decrypt licenseNumber
  pure
    Common.DriverLicenseAPIEntity
      { driverLicenseId = cast @DriverLicense @Common.DriverLicense id,
        documentImageId1 = cast @Image @Common.Image documentImageId1,
        documentImageId2 = (cast @Image @Common.Image) <$> documentImageId2,
        licenseNumber = licenseNumber',
        verificationStatus = castVerificationStatus verificationStatus,
        ..
      }

buildRCAssociationAPIEntity ::
  EncFlow m r =>
  (DriverRCAssociation, VehicleRegistrationCertificate) ->
  m Common.DriverRCAssociationAPIEntity
buildRCAssociationAPIEntity (DriverRCAssociation {..}, vehicleRC) = do
  details <- buildVehicleRCAPIEntity vehicleRC
  pure Common.DriverRCAssociationAPIEntity {..}

buildVehicleRCAPIEntity :: EncFlow m r => VehicleRegistrationCertificate -> m Common.VehicleRegistrationCertificateAPIEntity
buildVehicleRCAPIEntity VehicleRegistrationCertificate {..} = do
  certificateNumber' <- decrypt certificateNumber
  pure
    Common.VehicleRegistrationCertificateAPIEntity
      { registrationCertificateId = cast @VehicleRegistrationCertificate @Common.VehicleRegistrationCertificate id,
        documentImageId = cast @Image @Common.Image documentImageId,
        certificateNumber = certificateNumber',
        verificationStatus = castVerificationStatus verificationStatus,
        ..
      }

castVerificationStatus :: IV.VerificationStatus -> Common.VerificationStatus
castVerificationStatus = \case
  IV.PENDING -> Common.PENDING
  IV.VALID -> Common.VALID
  IV.INVALID -> Common.INVALID

---------------------------------------------------------------------
deleteDriver :: ShortId DM.Merchant -> Id Common.Driver -> Flow APISuccess
deleteDriver merchantShortId reqDriverId = do
  merchant <- findMerchantByShortId merchantShortId
  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById (Proxy @Flow) personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  unless (merchant.id == driver.merchantId) $ throwError (PersonDoesNotExist personId.getId)

  unless (driver.role == DP.DRIVER) $ throwError Unauthorized

  ride <- QRide.findOneByDriverId personId (Proxy @Flow)
  unless (isNothing ride) $
    throwError $ InvalidRequest "Unable to delete driver, which have at least one ride"

  driverInformation <- CQDriverInfo.findById driverId >>= fromMaybeM DriverInfoNotFound
  when driverInformation.enabled $
    throwError $ InvalidRequest "Driver should be disabled before deletion"

  -- this function uses tokens from db, so should be called before transaction
  Auth.clearDriverSession personId
  Esq.runTransaction $ do
    QIV.deleteByPersonId @Flow personId
    QImage.deleteByPersonId personId
    QDriverLicense.deleteByDriverId personId
    QRCAssociation.deleteByDriverId personId
    QDriverQuote.deleteByDriverId personId
    QSearchReqForDriver.deleteByDriverId personId
    QDriverStats.deleteById driverId
    QDriverLocation.deleteById personId
    QR.deleteByPersonId personId
    QVehicle.deleteById personId
    QDriverInfo.deleteById driverId
    QDriverFlowStatus.deleteById personId
    QPerson.deleteById personId
  CQDriverInfo.clearDriverInfoCache driverId
  logTagInfo "dashboard -> deleteDriver : " (show driverId)
  return Success

---------------------------------------------------------------------
unlinkVehicle :: ShortId DM.Merchant -> Id Common.Driver -> Flow APISuccess
unlinkVehicle merchantShortId reqDriverId = do
  merchant <- findMerchantByShortId merchantShortId

  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById (Proxy @Flow) personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  unless (merchant.id == driver.merchantId) $ throwError (PersonDoesNotExist personId.getId)

  Esq.runTransaction $ do
    QVehicle.deleteById @Flow personId
    QRCAssociation.endAssociation personId
  CQDriverInfo.updateEnabledVerifiedState driverId False False
  logTagInfo "dashboard -> unlinkVehicle : " (show personId)
  pure Success

---------------------------------------------------------------------
updatePhoneNumber :: ShortId DM.Merchant -> Id Common.Driver -> Common.UpdatePhoneNumberReq -> Flow APISuccess
updatePhoneNumber merchantShortId reqDriverId req = do
  runRequestValidation Common.validateUpdatePhoneNumberReq req
  merchant <- findMerchantByShortId merchantShortId

  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById (Proxy @Flow) personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  unless (merchant.id == driver.merchantId) $ throwError (PersonDoesNotExist personId.getId)
  phoneNumberHash <- getDbHash req.newPhoneNumber
  mbLinkedPerson <- QPerson.findByMobileNumber req.newCountryCode phoneNumberHash (Proxy @Flow)
  whenJust mbLinkedPerson $ \linkedPerson -> do
    if linkedPerson.id == driver.id
      then throwError $ InvalidRequest "Person already have the same mobile number"
      else throwError $ InvalidRequest "Person with this mobile number already exists"

  encNewPhoneNumber <- encrypt req.newPhoneNumber
  let updDriver =
        driver
          { DP.mobileCountryCode = Just req.newCountryCode,
            DP.mobileNumber = Just encNewPhoneNumber,
            DP.unencryptedMobileNumber = Just req.newPhoneNumber
          }
  -- this function uses tokens from db, so should be called before transaction
  Auth.clearDriverSession personId
  Esq.runTransaction $ do
    QPerson.updateMobileNumberAndCode @Flow updDriver
    QR.deleteByPersonId personId
  logTagInfo "dashboard -> updatePhoneNumber : " (show personId)
  pure Success

---------------------------------------------------------------------
addVehicle :: ShortId DM.Merchant -> Id Common.Driver -> Common.AddVehicleReq -> Flow APISuccess
addVehicle merchantShortId reqDriverId req = do
  runRequestValidation Common.validateAddVehicleReq req
  merchant <- findMerchantByShortId merchantShortId

  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById (Proxy @Flow) personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId) $ throwError (PersonDoesNotExist personId.getId)

  mbLinkedVehicle <- QVehicle.findById (Proxy @Flow) personId
  whenJust mbLinkedVehicle $ \_ -> throwError VehicleAlreadyLinked
  vehicle <- buildVehicle merchantId personId req
  let updDriver = driver {DP.firstName = req.driverName} :: DP.Person
  Esq.runTransaction $ do
    QVehicle.create @Flow vehicle
    QPerson.updatePersonRec personId updDriver
  logTagInfo "dashboard -> addVehicle : " (show personId)
  pure Success

buildVehicle :: MonadFlow m => Id DM.Merchant -> Id DP.Person -> Common.AddVehicleReq -> m DVeh.Vehicle
buildVehicle merchantId personId req = do
  now <- getCurrentTime
  return $
    DVeh.Vehicle
      { driverId = personId,
        merchantId = merchantId,
        variant = castVehicleVariant req.variant,
        model = req.model,
        color = req.colour,
        registrationNo = req.registrationNo,
        capacity = req.capacity,
        category = Nothing,
        make = req.make,
        size = Nothing,
        energyType = req.energyType,
        registrationCategory = Nothing,
        vehicleClass = req.vehicleClass,
        createdAt = now,
        updatedAt = now
      }
  where
    castVehicleVariant = \case
      Common.SUV -> DVeh.SUV
      Common.HATCHBACK -> DVeh.HATCHBACK
      Common.SEDAN -> DVeh.SEDAN
      Common.AUTO_RICKSHAW -> DVeh.AUTO_RICKSHAW

---------------------------------------------------------------------
updateDriverName :: ShortId DM.Merchant -> Id Common.Driver -> Common.UpdateDriverNameReq -> Flow APISuccess
updateDriverName merchantShortId reqDriverId req = do
  runRequestValidation Common.validateUpdateDriverNameReq req
  merchant <- findMerchantByShortId merchantShortId

  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    Esq.runInReplica (QPerson.findById (Proxy @Flow) personId)
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  unless (merchant.id == driver.merchantId) $ throwError (PersonDoesNotExist personId.getId)
  -- empty string in request condsidered as Nothing in db, Nothing in request is not affect db value
  let updDriver =
        driver{firstName = req.firstName,
               middleName = if req.middleName == Just "" then Nothing else req.middleName <|> driver.middleName,
               lastName = if req.lastName == Just "" then Nothing else req.lastName <|> driver.lastName
              }

  Esq.runTransaction $ do
    QPerson.updatePersonRec @Flow personId updDriver

  logTagInfo "dashboard -> updateDriverName : " (show personId)
  pure Success

---------------------------------------------------------------------
unlinkDL :: ShortId DM.Merchant -> Id Common.Driver -> Flow APISuccess
unlinkDL merchantShortId driverId = do
  merchant <- findMerchantByShortId merchantShortId

  let driverId_ = cast @Common.Driver @DP.Driver driverId
  let personId = cast @Common.Driver @DP.Person driverId

  driver <- Esq.runInReplica $ QPerson.findById (Proxy @Flow) personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- merchant access checking
  unless (merchant.id == driver.merchantId) $ throwError (PersonDoesNotExist personId.getId)

  Esq.runTransaction $ do
    QDriverLicense.deleteByDriverId @Flow personId
  CQDriverInfo.updateEnabledVerifiedState driverId_ False False
  logTagInfo "dashboard -> unlinkDL : " (show personId)
  pure Success

---------------------------------------------------------------------
endRCAssociation :: ShortId DM.Merchant -> Id Common.Driver -> Flow APISuccess
endRCAssociation merchantShortId reqDriverId = do
  merchant <- findMerchantByShortId merchantShortId

  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId

  driver <- Esq.runInReplica $ QPerson.findById (Proxy @Flow) personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- merchant access checking
  unless (merchant.id == driver.merchantId) $ throwError (PersonDoesNotExist personId.getId)

  Esq.runTransaction $ do
    QRCAssociation.endAssociation @Flow personId
  CQDriverInfo.updateEnabledVerifiedState driverId False False
  logTagInfo "dashboard -> endRCAssociation : " (show personId)
  pure Success
