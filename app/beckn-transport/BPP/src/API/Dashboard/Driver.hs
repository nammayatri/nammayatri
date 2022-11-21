module API.Dashboard.Driver where

import Beckn.External.Encryption (decrypt)
import Beckn.External.Maps.Types
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.APISuccess (APISuccess (..))
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Driver as Common
import Data.Coerce
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import Domain.Types.Person
import qualified Domain.Types.Person as DP
import Environment
import Servant hiding (Unauthorized, throwError)
import qualified Storage.CachedQueries.Merchant as QM
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
import Tools.Auth (authTokenCacheKey)
import Tools.Error

type API =
  "driver"
    :> ( Common.DriverListAPI
           :<|> Common.DriverActivityAPI
           :<|> Common.EnableDriversAPI
           :<|> Common.DisableDriversAPI
           :<|> Common.DriverLocationAPI
           :<|> Common.DriverInfoAPI
           :<|> Common.DeleteDriverAPI
       )

handler :: ShortId DM.Merchant -> FlowServer API
handler merchantId =
  listDrivers merchantId
    :<|> driverActivity merchantId
    :<|> enableDrivers merchantId
    :<|> disableDrivers merchantId
    :<|> driverLocation merchantId
    :<|> driverInfo merchantId
    :<|> deleteDriver merchantId

limitOffset :: Maybe Int -> Maybe Int -> [a] -> [a]
limitOffset mbLimit mbOffset =
  maybe identity take mbLimit . maybe identity drop mbOffset

-- TODO move all this logic to domain layer
listDrivers :: ShortId DM.Merchant -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> FlowHandler Common.DriverListRes
listDrivers merchantShortId mbLimit mbOffset _verified _rejected _pendingdoc mbSearchPhone = withFlowHandlerAPI $ do
  merchant <-
    QM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  items <- mapM buildDriverListItem =<< QPerson.findAllDriversFirstNameAsc merchant.id
  let filteredByPhone = filter (filterPhone . (.phoneNo)) items
      limitedItems = limitOffset mbLimit mbOffset filteredByPhone
  pure $ Common.DriverListRes (length limitedItems) limitedItems
  where
    filterPhone :: Text -> Bool
    filterPhone driverPhone = maybe True (`T.isInfixOf` driverPhone) mbSearchPhone

buildDriverListItem :: (EncFlow m r) => QPerson.FullDriver -> m Common.DriverListItem
buildDriverListItem f = do
  let p = f.person; drin = f.info; veh = f.vehicle
  phoneNo <- maybe (pure "") decrypt p.mobileNumber
  pure
    Common.DriverListItem
      { driverId = cast p.id,
        firstName = p.firstName,
        middleName = p.middleName,
        lastName = p.lastName,
        vehicleNo = Just veh.registrationNo,
        phoneNo,
        enabled = drin.enabled,
        verified = True,
        dlStatus = Nothing,
        rcStatus = Nothing
      }

driverActivity :: ShortId DM.Merchant -> FlowHandler Common.DriverActivityRes
driverActivity merchantShortId = withFlowHandlerAPI $ do
  merchant <-
    QM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  foldl' func Common.emptyDriverActivityRes <$> QPerson.findAllDriversFirstNameAsc merchant.id
  where
    func :: Common.DriverActivityRes -> QPerson.FullDriver -> Common.DriverActivityRes
    func acc x =
      if x.info.active
        then acc {Common.activeDriversInApp = acc.activeDriversInApp + 1}
        else acc {Common.inactiveDrivers = acc.inactiveDrivers + 1}

enableDrivers :: ShortId DM.Merchant -> Common.DriverIds -> FlowHandler Common.EnableDriversRes
enableDrivers merchantShortId req = withFlowHandlerAPI $ do
  merchant <-
    QM.findByShortId merchantShortId
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

disableDrivers :: ShortId DM.Merchant -> Common.DriverIds -> FlowHandler Common.DisableDriversRes
disableDrivers merchantShortId req = withFlowHandlerAPI $ do
  merchant <-
    QM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  let enable = False
  updatedDrivers <- QDriverInfo.updateEnabledStateReturningIds merchant.id (coerce req.driverIds) enable
  let driversNotFound = filter (not . (`elem` coerce @_ @[Id Common.Driver] updatedDrivers)) req.driverIds
  let numDriversDisabled = length updatedDrivers
  pure $
    Common.DisableDriversRes
      { numDriversDisabled,
        driversDisabled = coerce updatedDrivers,
        message =
          mconcat
            [ show numDriversDisabled,
              " drivers disabled, following drivers not found: ",
              show $ coerce @_ @[Text] driversNotFound
            ]
      }

driverLocation :: ShortId DM.Merchant -> Maybe Int -> Maybe Int -> Common.DriverIds -> FlowHandler Common.DriverLocationRes
driverLocation merchantShortId mbLimit mbOffset req = withFlowHandler $ do
  merchant <-
    QM.findByShortId merchantShortId
      >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  let driverIds = coerce req.driverIds
  allDrivers <- QPerson.findAllDriversByIdsFirstNameAsc merchant.id driverIds
  let driversNotFound =
        filter (not . (`elem` map ((.id) . (.person)) allDrivers)) driverIds
      limitedDrivers = limitOffset mbLimit mbOffset allDrivers
  resultList <- mapM buildDriverLocationListItem limitedDrivers
  pure $ Common.DriverLocationRes (nonEmpty $ coerce driversNotFound) resultList

buildDriverLocationListItem :: (EncFlow m r) => QPerson.FullDriver -> m Common.DriverLocationItem
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

-- FIXME Do we need to include mobileCountryCode into query params?
mobileIndianCode :: Text
mobileIndianCode = "+91"

driverInfo :: ShortId DM.Merchant -> Maybe Text -> Maybe Text -> FlowHandler Common.DriverInfoRes
driverInfo merchantShortId mbMobileNumber mbVehicleNumber = withFlowHandler $ do
  merchant <-
    QM.findByShortId merchantShortId
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

deleteDriver :: ShortId DM.Merchant -> Id Common.Driver -> FlowHandler APISuccess
deleteDriver merchantShortId reqDriverId = withFlowHandler $ do
  merchant <-
    QM.findByShortId merchantShortId
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

  clearDriverSession personId
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
  where
    clearDriverSession personId = do
      regTokens <- QR.findAllByPersonId personId
      for_ regTokens $ \regToken -> do
        void $ Redis.del $ authTokenCacheKey regToken.token
