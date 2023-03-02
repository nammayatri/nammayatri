{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Driver
  ( listDrivers,
    driverActivity,
    enableDriver,
    disableDriver,
    blockDriver,
    unblockDriver,
    driverLocation,
    driverInfo,
    deleteDriver,
    unlinkVehicle,
    updatePhoneNumber,
    addVehicle,
    updateDriverName,
  )
where

import Control.Applicative ((<|>))
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver as Common
import Data.Aeson (eitherDecode)
import Data.Coerce
import Data.Either.Combinators (mapLeft)
import Data.List.NonEmpty (nonEmpty)
import qualified Domain.Types.DriverInformation as DrInfo
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Vehicle as DVeh
import Environment
import Kernel.External.Encryption (decrypt, encrypt, getDbHash)
import Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.APISuccess (APISuccess (..))
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation (runRequestValidation)
import SharedLogic.Merchant (findMerchantByShortId)
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
  Maybe Bool ->
  Maybe Text ->
  Flow Common.DriverListRes
listDrivers merchantShortId mbLimit mbOffset mbVerified mbEnabled mbBlocked mbSearchPhone = do
  merchant <- findMerchantByShortId merchantShortId
  -- all drivers are considered as verified, because driverInfo.verified is not implemented for this bpp
  mbSearchPhoneDBHash <- getDbHash `traverse` mbSearchPhone
  driversWithInfo <-
    if mbVerified == Just True || isNothing mbVerified
      then do
        let limit = min maxLimit . fromMaybe defaultLimit $ mbLimit
            offset = fromMaybe 0 mbOffset
        Esq.runInReplica $ QPerson.findAllDriversWithInfoAndVehicle merchant.id limit offset mbEnabled mbBlocked mbSearchPhoneDBHash (Proxy @Flow)
      else pure []
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
        verified = True,
        onRide = driverInformation.onRide,
        active = driverInformation.active
      }

---------------------------------------------------------------------
driverActivity :: ShortId DM.Merchant -> Flow Common.DriverActivityRes
driverActivity merchantShortId = do
  merchant <- findMerchantByShortId merchantShortId
  Common.mkDriverActivityRes <$> Esq.runInReplica (QDriverInfo.countDrivers merchant.id (Proxy @Flow))

---------------------------------------------------------------------
enableDriver :: ShortId DM.Merchant -> Id Common.Driver -> Flow APISuccess
enableDriver merchantShortId reqDriverId = do
  merchant <- findMerchantByShortId merchantShortId

  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById personId (Proxy @Flow)
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId) $ throwError (PersonDoesNotExist personId.getId)

  _vehicle <- QVehicle.findById personId (Proxy @Flow) >>= fromMaybeM (VehicleDoesNotExist personId.getId)
  Esq.runTransaction $ do
    QDriverInfo.updateEnabledState @Flow driverId True
  logTagInfo "dashboard -> enableDriver : " (show personId)
  pure Success

---------------------------------------------------------------------
disableDriver :: ShortId DM.Merchant -> Id Common.Driver -> Flow APISuccess
disableDriver merchantShortId reqDriverId = do
  merchant <- findMerchantByShortId merchantShortId

  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById personId (Proxy @Flow)
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId) $ throwError (PersonDoesNotExist personId.getId)

  Esq.runTransaction $ do
    QVehicle.deleteById @Flow personId
    QDriverInfo.updateEnabledState driverId False
  logTagInfo "dashboard -> disableDriver : " (show personId)
  pure Success

---------------------------------------------------------------------
blockDriver :: ShortId DM.Merchant -> Id Common.Driver -> Flow APISuccess
blockDriver merchantShortId reqDriverId = do
  merchant <- findMerchantByShortId merchantShortId

  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    Esq.runInReplica (QPerson.findById personId (Proxy @Flow))
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId) $ throwError (PersonDoesNotExist personId.getId)

  Esq.runTransaction $ QDriverInfo.updateBlockedState @Flow driverId True
  logTagInfo "dashboard -> blockDriver : " (show personId)
  pure Success

---------------------------------------------------------------------
unblockDriver :: ShortId DM.Merchant -> Id Common.Driver -> Flow APISuccess
unblockDriver merchantShortId reqDriverId = do
  merchant <- findMerchantByShortId merchantShortId

  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    Esq.runInReplica (QPerson.findById personId (Proxy @Flow))
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId) $ throwError (PersonDoesNotExist personId.getId)

  Esq.runTransaction $ QDriverInfo.updateBlockedState @Flow driverId False
  logTagInfo "dashboard -> unblockDriver : " (show personId)
  pure Success

---------------------------------------------------------------------
driverLocation ::
  ShortId DM.Merchant ->
  Maybe Int ->
  Maybe Int ->
  Common.DriverIds ->
  Flow Common.DriverLocationRes
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
  when (isJust mbDlNumber) $
    throwError $ InvalidRequest "\"dlNumber\" is not required for static offer driver app"
  when (isJust mbRcNumber) $
    throwError $ InvalidRequest "\"rcNumber\" is not required for static offer driver app"
  when (isJust mbMobileCountryCode && isNothing mbMobileNumber) $
    throwError $ InvalidRequest "\"mobileCountryCode\" can be used only with \"mobileNumber\""
  merchant <- findMerchantByShortId merchantShortId
  driverDocsInfo <- case (mbMobileNumber, mbVehicleNumber) of
    (Just mobileNumber, Nothing) -> do
      mobileNumberDbHash <- getDbHash mobileNumber
      let mobileCountryCode = fromMaybe mobileIndianCode mbMobileCountryCode
      Esq.runInReplica $
        QPerson.fetchDriverInfoWithRidesCount merchant.id (Just (mobileNumberDbHash, mobileCountryCode)) Nothing (Proxy @Flow)
          >>= fromMaybeM (PersonDoesNotExist $ mobileCountryCode <> mobileNumber)
    (Nothing, Just vehicleNumber) ->
      Esq.runInReplica $
        QPerson.fetchDriverInfoWithRidesCount merchant.id Nothing (Just vehicleNumber) (Proxy @Flow)
          >>= fromMaybeM (VehicleDoesNotExist vehicleNumber)
    _ -> throwError $ InvalidRequest "Exactly one of query parameters \"mobileNumber\", \"vehicleNumber\" is required"
  buildDriverInfoRes driverDocsInfo
  where
    buildDriverInfoRes :: EncFlow m r => QPerson.DriverWithRidesCount -> m Common.DriverInfoRes
    buildDriverInfoRes QPerson.DriverWithRidesCount {..} = do
      mobileNumber <- traverse decrypt person.mobileNumber
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
            verified = True, -- not implemented for this bpp
            vehicleNumber = vehicle <&> (.registrationNo),
            driverLicenseDetails = Nothing, -- not implemented for this bpp
            vehicleRegistrationDetails = [] -- not implemented for this bpp
          }

---------------------------------------------------------------------
deleteDriver :: ShortId DM.Merchant -> Id Common.Driver -> Flow APISuccess
deleteDriver merchantShortId reqDriverId = do
  merchant <- findMerchantByShortId merchantShortId
  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById personId (Proxy @Flow)
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId) $ throwError (PersonDoesNotExist personId.getId)

  unless (driver.role == DP.DRIVER) $ throwError Unauthorized

  rides <- QRide.findOneByDriverId personId (Proxy @Flow)
  unless (isNothing rides) $
    throwError $ InvalidRequest "Unable to delete driver, which have at least one ride"

  driverInformation <- QDriverInfo.findById driverId (Proxy @Flow) >>= fromMaybeM DriverInfoNotFound
  when driverInformation.enabled $
    throwError $ InvalidRequest "Driver should be disabled before deletion"

  -- this function uses tokens from db, so should be called before transaction
  Auth.clearDriverSession personId
  Esq.runTransaction $ do
    QNotificationStatus.deleteByPersonId @Flow driverId
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
  merchant <- findMerchantByShortId merchantShortId

  let driverId = cast @Common.Driver @DP.Driver reqDriverId
  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById personId (Proxy @Flow)
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId) $ throwError (PersonDoesNotExist personId.getId)

  Esq.runTransaction $ do
    QVehicle.deleteById @Flow personId
    QDriverInfo.updateEnabledState driverId False
  logTagInfo "dashboard -> unlinkVehicle : " (show personId)
  return Success

---------------------------------------------------------------------
updatePhoneNumber :: ShortId DM.Merchant -> Id Common.Driver -> Common.UpdatePhoneNumberReq -> Flow APISuccess
updatePhoneNumber merchantShortId reqDriverId req = do
  runRequestValidation Common.validateUpdatePhoneNumberReq req
  merchant <- findMerchantByShortId merchantShortId

  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    QPerson.findById personId (Proxy @Flow)
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId) $ throwError (PersonDoesNotExist personId.getId)
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
            DP.mobileNumber = Just encNewPhoneNumber
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
    QPerson.findById personId (Proxy @Flow)
      >>= fromMaybeM (PersonDoesNotExist personId.getId)

  -- merchant access checking
  let merchantId = driver.merchantId
  unless (merchant.id == merchantId) $ throwError (PersonDoesNotExist personId.getId)

  mbLinkedVehicle <- QVehicle.findById personId (Proxy @Flow)
  whenJust mbLinkedVehicle $ \_ -> throwError VehicleAlreadyLinked
  vehicle <- buildVehicle merchantId personId req
  let updDriver = driver {DP.firstName = req.driverName} :: DP.Person
  Esq.runTransaction $ do
    QVehicle.create @Flow vehicle
    QPerson.updatePersonRec personId updDriver
  logTagInfo "dashboard -> addVehicle : " (show personId)
  pure Success

-- TODO add vehicleClass
buildVehicle :: MonadFlow m => Id DM.Merchant -> Id DP.Person -> Common.AddVehicleReq -> m DVeh.Vehicle
buildVehicle merchantId personId req = do
  now <- getCurrentTime
  variant <- fromEitherM InvalidRequest $ castVehicleVariant req.variant
  mbEnergyType <- forM req.energyType $ fromEitherM InvalidRequest . castEnergyType
  return $
    DVeh.Vehicle
      { driverId = personId,
        merchantId = merchantId,
        variant,
        model = req.model,
        color = req.colour,
        registrationNo = req.registrationNo,
        capacity = req.capacity,
        category = Nothing,
        make = req.make,
        size = Nothing,
        energyType = mbEnergyType,
        registrationCategory = Nothing,
        vehicleClass = req.vehicleClass,
        createdAt = now,
        updatedAt = now
      }
  where
    castVehicleVariant = \case
      Common.SUV -> Right DVeh.SUV
      Common.HATCHBACK -> Right DVeh.HATCHBACK
      Common.SEDAN -> Right DVeh.SEDAN
      Common.AUTO_RICKSHAW -> Left "Auto-rickshaw vehicles are not supported by this BPP"

    castEnergyType :: Text -> Either Text DVeh.EnergyType
    castEnergyType = mapLeft show . eitherDecode . show

---------------------------------------------------------------------
updateDriverName :: ShortId DM.Merchant -> Id Common.Driver -> Common.UpdateDriverNameReq -> Flow APISuccess
updateDriverName merchantShortId reqDriverId req = do
  runRequestValidation Common.validateUpdateDriverNameReq req
  merchant <- findMerchantByShortId merchantShortId

  let personId = cast @Common.Driver @DP.Person reqDriverId
  driver <-
    Esq.runInReplica (QPerson.findById personId (Proxy @Flow))
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
