module API.Dashboard.Driver where

import Beckn.External.Encryption (decrypt)
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Types.MapSearch
import Beckn.Utils.Common
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Driver as Common
import Data.Coerce
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Text as T
import Domain.Types.Person
import Environment
import GHC.TypeLits
import Servant hiding (throwError)
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.Person as QPerson
import Tools.Auth (Dashboard, DashboardTokenAuth)

type API =
  DriverListAPI
    :<|> DriverActivityAPI
    :<|> EnableDriversAPI
    :<|> DisableDriversAPI
    :<|> DriverLocationAPI

handler :: FlowServer API
handler =
  listDrivers
    :<|> driverActivity
    :<|> enableDrivers
    :<|> disableDrivers
    :<|> driverLocation

type FromCommon (t :: Symbol) api =
  "driver"
    :> DashboardTokenAuth
    :> t
    :> api

type DriverListAPI = FromCommon "list" Common.DriverListAPI

type DriverActivityAPI = FromCommon "activity" Common.DriverActivityAPI

type EnableDriversAPI = FromCommon "enable" Common.EnableDriversAPI

type DisableDriversAPI = FromCommon "disable" Common.DisableDriversAPI

type DriverLocationAPI = FromCommon "location" Common.DriverLocationAPI

listDrivers :: Dashboard -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> FlowHandler Common.DriverListRes
listDrivers _ _mbLimit _mbOffset _verified _rejected _pendingdoc mbSearchPhone = withFlowHandlerAPI $ do
  items <- mapM buildDriverListItem =<< QPerson.findAllDrivers
  let filteredByPhone = filter (filterPhone . (.phoneNo)) items
  pure $ Common.DriverListRes (length filteredByPhone) filteredByPhone
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
        vehicleNo = veh.registrationNo,
        phoneNo,
        enabled = drin.enabled,
        verified = True,
        dlStatus = Nothing,
        rcStatus = Nothing
      }

driverActivity :: Dashboard -> FlowHandler Common.DriverActivityRes
driverActivity _ = withFlowHandlerAPI $ do
  foldl' func Common.emptyDriverActivityRes <$> QPerson.findAllDrivers
  where
    func :: Common.DriverActivityRes -> QPerson.FullDriver -> Common.DriverActivityRes
    func acc x =
      if x.info.active
        then acc {Common.activeDriversInApp = acc.activeDriversInApp + 1}
        else acc {Common.inactiveDrivers = acc.inactiveDrivers + 1}

enableDrivers :: Dashboard -> Common.DriverIds -> FlowHandler Common.EnableDriversRes
enableDrivers _ req = withFlowHandlerAPI $ do
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

disableDrivers :: Dashboard -> Common.DriverIds -> FlowHandler Common.DisableDriversRes
disableDrivers _ req = withFlowHandlerAPI $ do
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

driverLocation :: Dashboard -> Common.DriverIds -> FlowHandler Common.DriverLocationRes
driverLocation _ req = withFlowHandler $ do
  let driverIds = coerce req.driverIds
  allDrivers <- QPerson.findAllDriversByIds driverIds
  let driversNotFound =
        filter (not . (`elem` map ((.id) . (.person)) allDrivers)) driverIds
  resultList <- mapM buildDriverLocationListItem allDrivers
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
