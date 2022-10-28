module API.Dashboard.Driver where

import Beckn.External.Encryption (decrypt)
import Beckn.External.Maps.Types
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Driver as Common
import Data.Coerce
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Text as T
import Domain.Types.Person
import Environment
import Servant hiding (throwError)
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.Person as QPerson

type API =
  "driver"
    :> ( DriverListAPI
           :<|> DriverActivityAPI
           :<|> EnableDriversAPI
           :<|> DisableDriversAPI
           :<|> DriverLocationAPI
       )

type DriverListAPI = "list" :> Common.DriverListAPI

type DriverActivityAPI = "activity" :> Common.DriverActivityAPI

type EnableDriversAPI = "enable" :> Common.EnableDriversAPI

type DisableDriversAPI = "disable" :> Common.DisableDriversAPI

type DriverLocationAPI = "location" :> Common.DriverLocationAPI

handler :: FlowServer API
handler =
  listDrivers
    :<|> driverActivity
    :<|> enableDrivers
    :<|> disableDrivers
    :<|> driverLocation

limitOffset :: Maybe Int -> Maybe Int -> [a] -> [a]
limitOffset mbLimit mbOffset =
  maybe identity take mbLimit . maybe identity drop mbOffset

listDrivers :: Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> FlowHandler Common.DriverListRes
listDrivers mbLimit mbOffset _verified _rejected _pendingdoc mbSearchPhone = withFlowHandlerAPI $ do
  items <- mapM buildDriverListItem =<< QPerson.findAllDriversFirstnameAsc
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

driverActivity :: FlowHandler Common.DriverActivityRes
driverActivity = withFlowHandlerAPI $ do
  foldl' func Common.emptyDriverActivityRes <$> QPerson.findAllDrivers
  where
    func :: Common.DriverActivityRes -> QPerson.FullDriver -> Common.DriverActivityRes
    func acc x =
      if x.info.active
        then acc {Common.activeDriversInApp = acc.activeDriversInApp + 1}
        else acc {Common.inactiveDrivers = acc.inactiveDrivers + 1}

enableDrivers :: Common.DriverIds -> FlowHandler Common.EnableDriversRes
enableDrivers req = withFlowHandlerAPI $ do
  let enable = True
  updatedDrivers <- QDriverInfo.updateEnabledStateReturningIds (coerce req.driverIds) enable
  let driversNotFound = filter (not . (`elem` coerce @[Id Driver] @[Id Common.Driver] updatedDrivers)) req.driverIds
  let numDriversEnabled = length updatedDrivers
  pure $
    Common.EnableDriversRes
      { numDriversEnabled,
        driversEnabled = coerce updatedDrivers,
        message = mconcat [show numDriversEnabled, " drivers enabled, following drivers not found: ", show $ coerce @_ @[Text] driversNotFound]
      }

disableDrivers :: Common.DriverIds -> FlowHandler Common.DisableDriversRes
disableDrivers req = withFlowHandlerAPI $ do
  let enable = False
  updatedDrivers <- QDriverInfo.updateEnabledStateReturningIds (coerce req.driverIds) enable
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

driverLocation :: Maybe Int -> Maybe Int -> Common.DriverIds -> FlowHandler Common.DriverLocationRes
driverLocation mbLimit mbOffset req = withFlowHandler $ do
  let driverIds = coerce req.driverIds
  allDrivers <- QPerson.findAllDriversByIdsFirstnameAsc driverIds
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
