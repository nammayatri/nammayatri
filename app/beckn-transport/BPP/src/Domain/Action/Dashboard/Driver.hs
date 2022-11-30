module Domain.Action.Dashboard.Driver
  ( listDrivers,
    driverActivity,
    enableDriver,
    disableDriver,
    driverLocation,
    driverInfo,
    deleteDriver,
    unlinkVehicle,
    updatePhoneNumber,
  )
where

import Beckn.External.Encryption (decrypt, encrypt)
import Beckn.External.Maps.Types
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.APISuccess (APISuccess (..))
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Validation (runRequestValidation)
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Driver as Common
import Data.Coerce
import Data.List.NonEmpty (nonEmpty)
import qualified Domain.Types.DriverInformation as DrInfo
import qualified Domain.Types.Merchant as DM
import Domain.Types.Person
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Vehicle as DVeh
import Environment
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.AllocationEvent as QAllocationEvent
import qualified Storage.Queries.BusinessEvent as QBusinessEvent
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.DriverLocation as QDriverLocation
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.NotificationStatus as QNotificationStatus
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Tools.Auth as Auth
import Tools.Error

-- FIXME remove this, all entities should be limited on db level
limitOffset :: Maybe Int -> Maybe Int -> [a] -> [a]
limitOffset mbLimit mbOffset =
  maybe identity take mbLimit . maybe identity drop mbOffset

---------------------------------------------------------------------
listDrivers ::
  ShortId DM.Merchant ->
  Maybe Int ->
  Maybe Int ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe Text ->
  Flow Common.DriverListRes
listDrivers merchantShortId mbLimit mbOffset mbVerified mbEnabled mbSearchPhone = do
  merchant <-
    CQM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  -- all drivers are considered as verified, because driverInfo.verified is not implemented for this bpp
  driversWithInfo <-
    if mbVerified == Just True || isNothing mbVerified
      then do
        let limit = min maxLimit . fromMaybe defaultLimit $ mbLimit
            offset = fromMaybe 0 mbOffset
        QPerson.findAllDriversWithInfoAndVehicle merchant.id limit offset mbEnabled mbSearchPhone
      else pure []
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
        verified = True,
        onRide = driverInformation.onRide,
        active = driverInformation.active
      }

---------------------------------------------------------------------
driverActivity :: ShortId DM.Merchant -> Flow Common.DriverActivityRes
driverActivity merchantShortId = do
  merchant <-
    CQM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  Common.mkDriverActivityRes <$> QDriverInfo.countDrivers merchant.id

---------------------------------------------------------------------
enableDriver :: ShortId DM.Merchant -> Id Common.Driver -> Flow APISuccess
enableDriver merchantShortId reqDriverId = do
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

  _vehicle <- QVehicle.findById personId >>= fromMaybeM (VehicleDoesNotExist personId.getId)
  Esq.runTransaction $ do
    QDriverInfo.updateEnabledState driverId True
  logTagInfo "dashboard -> enableDriver : " (show personId)
  pure Success

---------------------------------------------------------------------
disableDriver :: ShortId DM.Merchant -> Id Common.Driver -> Flow APISuccess
disableDriver merchantShortId reqDriverId = do
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

  Esq.runTransaction $ do
    QVehicle.deleteById personId
    QDriverInfo.updateEnabledState driverId False
  logTagInfo "dashboard -> disableDriver : " (show personId)
  pure Success

---------------------------------------------------------------------
driverLocation ::
  ShortId DM.Merchant ->
  Maybe Int ->
  Maybe Int ->
  Common.DriverIds ->
  Flow Common.DriverLocationRes
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
      QPerson.fetchFullDriverByMobileNumber merchant.id mobileNumber mobileIndianCode
        >>= fromMaybeM (PersonDoesNotExist $ mobileIndianCode <> mobileNumber)
    (Nothing, Just vehicleNumber) ->
      QPerson.fetchFullDriverInfoByVehNumber merchant.id vehicleNumber
        >>= fromMaybeM (VehicleDoesNotExist vehicleNumber)
    _ -> throwError $ InvalidRequest "Exactly one of query parameters \"mobileNumber\", \"vehicleNumber\" is required"
  buildDriverInfoRes driverDocsInfo
  where
    buildDriverInfoRes :: EncFlow m r => QPerson.DriverWithRidesCount -> m Common.DriverInfoRes
    buildDriverInfoRes QPerson.DriverWithRidesCount {..} = do
      mobileNumber <- traverse decrypt person.mobileNumber
      let vehicleDetails = mkVehicleAPIEntity vehicle.registrationNo
      pure
        Common.DriverInfoRes
          { driverId = cast @Person @Common.Driver person.id,
            firstName = person.firstName,
            middleName = person.middleName,
            lastName = person.lastName,
            dlNumber = Nothing, -- not implemented for this bpp
            dateOfBirth = Nothing, -- not implemented for this bpp
            numberOfRides = fromMaybe 0 ridesCount,
            mobileNumber,
            enabled = info.enabled,
            verified = True, -- not implemented for this bpp
            vehicleDetails = Just vehicleDetails
          }

    mkVehicleAPIEntity :: Text -> Common.VehicleAPIEntity
    mkVehicleAPIEntity vehicleNumber = do
      Common.VehicleAPIEntity
        { vehicleNumber,
          dateOfReg = Nothing, -- not implemented for this bpp,
          vehicleClass = Nothing -- not implemented for this bpp
        }

---------------------------------------------------------------------
deleteDriver :: ShortId DM.Merchant -> Id Common.Driver -> Flow APISuccess
deleteDriver merchantShortId reqDriverId = do
  merchant <-
    CQM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  merchantId <- driver.merchantId & fromMaybeM (PersonFieldNotPresent "merchant_id")
  unless (merchant.id == merchantId) $ throwError (PersonDoesNotExist personId.getId)

  unless (driver.role == DP.DRIVER) $ throwError Unauthorized

  rides <- QRide.findOneByDriverId personId
  unless (isNothing rides) $
    throwError $ InvalidRequest "Unable to delete driver, which have at least one ride"

  driverInformation <- QDriverInfo.findById driverId >>= fromMaybeM DriverInfoNotFound
  when driverInformation.enabled $
    throwError $ InvalidRequest "Driver should be disabled before deletion"

  -- this function uses tokens from db, so should be called before transaction
  Auth.clearDriverSession personId
  Esq.runTransaction $ do
    QNotificationStatus.deleteByPersonId driverId
    QAllocationEvent.deleteByPersonId driverId
    QBusinessEvent.deleteByPersonId driverId
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

  let driverId = cast @Common.Driver @Driver reqDriverId
  let personId = cast @Common.Driver @Person reqDriverId
  driver <-
    QPerson.findById personId
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  merchantId <- driver.merchantId & fromMaybeM (PersonFieldNotPresent "merchant_id")
  unless (merchant.id == merchantId) $ throwError (PersonDoesNotExist personId.getId)

  Esq.runTransaction $ do
    QVehicle.deleteById personId
    QDriverInfo.updateEnabledState driverId False
  logTagInfo "dashboard -> unlinkVehicle : " (show personId)
  return Success

---------------------------------------------------------------------
updatePhoneNumber :: ShortId DM.Merchant -> Id Common.Driver -> Common.UpdatePhoneNumberReq -> Flow APISuccess
updatePhoneNumber merchantShortId reqDriverId req = do
  runRequestValidation Common.validateUpdatePhoneNumberReq req
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

  mbLinkedPerson <- QPerson.findByMobileNumber req.newCountryCode req.newPhoneNumber
  whenJust mbLinkedPerson $ \linkedPerson -> do
    if linkedPerson.id == driver.id
      then throwError $ InvalidRequest "Person already have the same mobile number"
      else throwError $ InvalidRequest "Person with this mobile number already exists"

  encNewPhoneNumber <- encrypt req.newPhoneNumber
  let updDriver =
        driver
          { mobileCountryCode = Just req.newCountryCode,
            mobileNumber = Just encNewPhoneNumber
          }
  -- this function uses tokens from db, so should be called before transaction
  Auth.clearDriverSession personId
  Esq.runTransaction $ do
    QPerson.updateMobileNumberAndCode updDriver
    QR.deleteByPersonId personId
  logTagInfo "dashboard -> updatePhoneNumber : " (show personId)
  pure Success
