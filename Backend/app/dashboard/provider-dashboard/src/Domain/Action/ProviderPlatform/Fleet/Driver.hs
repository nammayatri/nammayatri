module Domain.Action.ProviderPlatform.Fleet.Driver
  ( postDriverFleetAddVehicle,
    postDriverFleetAddRCWithoutDriver,
    getDriverFleetGetAllVehicle,
    getDriverFleetGetAllDriver,
    postDriverFleetUnlink,
    postDriverFleetRemoveVehicle,
    postDriverFleetRemoveDriver,
    getDriverFleetTotalEarning,
    getDriverFleetVehicleEarning,
    getDriverFleetDriverEarning,
    getDriverFleetDriverVehicleAssociation,
    getDriverFleetDriverAssociation,
    getDriverFleetVehicleAssociation,
    postDriverFleetVehicleDriverRcStatus,
    postDriverUpdateFleetOwnerInfo,
    getDriverFleetOwnerInfo,
    postDriverFleetSendJoiningOtp,
    postDriverFleetVerifyJoiningOtp,
    postDriverFleetLinkRCWithDriver,
    getDriverFleetGetDriverRequests,
    postDriverFleetRespondDriverRequest,
    postDriverFleetAddVehicles,
    putDriverDashboardFleetWmbTripDelete,
    postDriverFleetAddDrivers,
    getDriverFleetRoutes,
    getDriverFleetPossibleRoutes,
    postDriverFleetTripPlanner,
    getDriverFleetTripTransactions,
    postDriverFleetAddDriverBusRouteMapping,
    getDriverDashboardFleetTrackDriver,
  )
where

import qualified API.Client.ProviderPlatform.Fleet as Client
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as Common
import qualified "dashboard-helper-api" Dashboard.Common as DCommon
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Management.DriverRegistration as Registration
import Data.Text
import "lib-dashboard" Domain.Action.Dashboard.Person as DPerson
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import EulerHS.Prelude
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation (runRequestValidation)
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import qualified Storage.CachedQueries.FleetMemberAssociation as FMA
import "lib-dashboard" Storage.Queries.Person as QP
import Tools.Auth.Api
import Tools.Auth.Merchant

buildTransaction ::
  ( MonadFlow m,
    Common.HideSecrets request
  ) =>
  ApiTokenInfo ->
  Maybe (Id Common.Driver) ->
  Maybe request ->
  m DT.Transaction
buildTransaction apiTokenInfo driverId =
  T.buildTransaction (DT.castEndpoint apiTokenInfo.userActionType) (Just DRIVER_OFFER_BPP_MANAGEMENT) (Just apiTokenInfo) driverId Nothing

getFleetOwnerId :: Text -> Flow Text
getFleetOwnerId memberPersonId = do
  fleetMemberAssociation <- FMA.findByPrimaryKey memberPersonId
  pure $ maybe memberPersonId (.fleetOwnerId) fleetMemberAssociation

postDriverFleetAddVehicle :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> Maybe Text -> Common.AddVehicleReq -> Flow APISuccess
postDriverFleetAddVehicle merchantShortId opCity apiTokenInfo phoneNo mbMobileCountryCode req = do
  runRequestValidation Common.validateAddVehicleReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetAddVehicle) phoneNo fleetOwnerId mbMobileCountryCode req

postDriverFleetAddRCWithoutDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Registration.RegisterRCReq -> Flow APISuccess
postDriverFleetAddRCWithoutDriver merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetAddRCWithoutDriver) fleetOwnerId req

getDriverFleetGetAllVehicle :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe Text -> Flow Common.ListVehicleRes
getDriverFleetGetAllVehicle merchantShortId opCity apiTokenInfo mbLimit mbOffset mbRegNumberString = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetGetAllVehicle) fleetOwnerId mbLimit mbOffset mbRegNumberString

getDriverFleetGetAllDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Flow Common.FleetListDriverRes
getDriverFleetGetAllDriver merchantShortId opCity apiTokenInfo mbLimit mbOffset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetGetAllDriver) fleetOwnerId mbLimit mbOffset

postDriverFleetUnlink :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Text -> Flow APISuccess
postDriverFleetUnlink merchantShortId opCity apiTokenInfo driverId vehicleNo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetUnlink) fleetOwnerId driverId vehicleNo

postDriverFleetRemoveVehicle :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> Flow APISuccess
postDriverFleetRemoveVehicle merchantShortId opCity apiTokenInfo vehicleNo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetRemoveVehicle) fleetOwnerId vehicleNo

postDriverFleetRemoveDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow APISuccess
postDriverFleetRemoveDriver merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetRemoveDriver) fleetOwnerId driverId

getDriverFleetTotalEarning :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe UTCTime -> Maybe UTCTime -> Flow Common.FleetTotalEarningResponse
getDriverFleetTotalEarning merchantShortId opCity apiTokenInfo mbFrom mbTo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetTotalEarning) fleetOwnerId mbFrom mbTo

getDriverFleetVehicleEarning :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe UTCTime -> Maybe UTCTime -> Flow Common.FleetEarningListRes
getDriverFleetVehicleEarning merchantShortId opCity apiTokenInfo mbVehicleNo mbLimit mbOffset mbFrom mbTo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetVehicleEarning) fleetOwnerId mbVehicleNo mbLimit mbOffset mbFrom mbTo

getDriverFleetDriverEarning :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe UTCTime -> Maybe UTCTime -> Maybe Bool -> Maybe Common.SortOn -> Flow Common.FleetEarningListRes
getDriverFleetDriverEarning merchantShortId opCity apiTokenInfo mbMobileCountryCode mbMobileNo mbLimit mbOffset mbFrom mbTo mbSortDesc mbSortOn = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetDriverEarning) fleetOwnerId mbMobileCountryCode mbMobileNo mbLimit mbOffset mbFrom mbTo mbSortDesc mbSortOn

getDriverFleetDriverVehicleAssociation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Flow Common.DrivertoVehicleAssociationRes
getDriverFleetDriverVehicleAssociation merchantId opCity apiTokenInfo mbLimit mbOffset mbCountryCode mbPhoneNo mbVehicleNo mbStatus mbFrom mbTo = do
  checkedMerchantId <- merchantCityAccessCheck merchantId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetDriverVehicleAssociation) fleetOwnerId mbLimit mbOffset mbCountryCode mbPhoneNo mbVehicleNo mbStatus mbFrom mbTo

getDriverFleetDriverAssociation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Maybe Common.DriverMode -> Flow Common.DrivertoVehicleAssociationRes
getDriverFleetDriverAssociation merhcantId opCity apiTokenInfo mbIsActive mbLimit mbOffset mbCountryCode mbPhoneNo mbStats mbFrom mbTo mbMode = do
  checkedMerchantId <- merchantCityAccessCheck merhcantId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetDriverAssociation) fleetOwnerId mbIsActive mbLimit mbOffset mbCountryCode mbPhoneNo mbStats mbFrom mbTo mbMode

getDriverFleetVehicleAssociation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Maybe Common.FleetVehicleStatus -> Flow Common.DrivertoVehicleAssociationRes
getDriverFleetVehicleAssociation merhcantId opCity apiTokenInfo mbLimit mbOffset mbVehicleNo mbIncludeStats mbFrom mbTo mbStatus = do
  checkedMerchantId <- merchantCityAccessCheck merhcantId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetVehicleAssociation) fleetOwnerId mbLimit mbOffset mbVehicleNo mbIncludeStats mbFrom mbTo mbStatus

postDriverFleetVehicleDriverRcStatus :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.RCStatusReq -> Flow APISuccess
postDriverFleetVehicleDriverRcStatus merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) $ Just req
  T.withTransactionStoring transaction $ do
    fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
    Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetVehicleDriverRcStatus) driverId fleetOwnerId req

postDriverUpdateFleetOwnerInfo :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Common.UpdateFleetOwnerInfoReq -> Flow APISuccess
postDriverUpdateFleetOwnerInfo merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Just req)
  T.withTransactionStoring transaction $ do
    fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
    unless (apiTokenInfo.personId.getId == driverId.getId && fleetOwnerId == apiTokenInfo.personId.getId) $
      throwError AccessDenied
    _ <- Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverUpdateFleetOwnerInfo) (Id fleetOwnerId) req
    let updateDriverReq =
          DPerson.UpdatePersonReq
            { firstName = req.firstName,
              lastName = req.lastName,
              email = req.email,
              mobileCountryCode = req.mobileCountryCode,
              mobileNumber = req.mobileNo
            }
    _ <- DPerson.updatePerson apiTokenInfo.personId updateDriverReq
    pure Success

getDriverFleetOwnerInfo :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow Common.FleetOwnerInfoRes
getDriverFleetOwnerInfo merchantShortId opCity apiTokenInfo driverId = do
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  unless (fleetOwnerId == driverId.getId) $
    throwError AccessDenied
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetOwnerInfo) driverId

postDriverFleetSendJoiningOtp :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Registration.AuthReq -> Flow Registration.AuthRes
postDriverFleetSendJoiningOtp merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  person <- QP.findById apiTokenInfo.personId >>= fromMaybeM (PersonNotFound fleetOwnerId)
  let dashboardUserName = person.firstName <> " " <> person.lastName
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetSendJoiningOtp) dashboardUserName req

postDriverFleetVerifyJoiningOtp :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Common.VerifyFleetJoiningOtpReq -> Flow APISuccess
postDriverFleetVerifyJoiningOtp merchantShortId opCity apiTokenInfo mbAuthId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetVerifyJoiningOtp) fleetOwnerId mbAuthId req

postDriverFleetLinkRCWithDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.LinkRCWithDriverForFleetReq -> Flow APISuccess
postDriverFleetLinkRCWithDriver merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetLinkRCWithDriver) fleetOwnerId req

postDriverFleetAddVehicles :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.CreateVehiclesReq -> Flow APISuccess
postDriverFleetAddVehicles merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  T.withTransactionStoring transaction $ Client.callFleetAPI checkedMerchantId opCity (Common.addMultipartBoundary "XXX00XXX" . (.driverDSL.postDriverFleetAddVehicles)) req{fleetOwnerId = Just fleetOwnerId}

getDriverFleetGetDriverRequests :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe UTCTime -> Maybe UTCTime -> Maybe Common.RequestStatus -> Maybe Int -> Maybe Int -> Flow Common.DriverRequestResp
getDriverFleetGetDriverRequests merchantShortId opCity apiTokenInfo mbFrom mbTo mbStatus mbLimit mbOffset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetGetDriverRequests) fleetOwnerId mbFrom mbTo mbStatus mbLimit mbOffset

postDriverFleetRespondDriverRequest :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.RequestRespondReq -> Flow APISuccess
postDriverFleetRespondDriverRequest merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing $ Just req
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  T.withTransactionStoring transaction $
    Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetRespondDriverRequest) fleetOwnerId req

putDriverDashboardFleetWmbTripDelete :: (ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.TripTransaction -> Flow APISuccess)
putDriverDashboardFleetWmbTripDelete merchantShortId opCity apiTokenInfo tripTransactionId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just $ DCommon.TransactionLogId tripTransactionId.getId)
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  T.withTransactionStoring transaction $ Client.callFleetAPI checkedMerchantId opCity (.driverDSL.putDriverDashboardFleetWmbTripDelete) tripTransactionId fleetOwnerId

getDriverFleetRoutes :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe LatLong -> Maybe Text -> Flow Common.RouteAPIResp
getDriverFleetRoutes merchantShortId opCity apiTokenInfo mbCurrentLocation mbSearchString = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetRoutes) fleetOwnerId mbCurrentLocation mbSearchString

getDriverFleetPossibleRoutes :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> LatLong -> Flow Common.RouteAPIResp
getDriverFleetPossibleRoutes merchantShortId opCity apiTokenInfo startLocation = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetPossibleRoutes) fleetOwnerId startLocation

postDriverFleetTripPlanner :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Text -> Common.TripPlannerReq -> Flow APISuccess
postDriverFleetTripPlanner merchantShortId opCity apiTokenInfo driverId vehicleNo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Just req)
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  T.withTransactionStoring transaction $ Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetTripPlanner) fleetOwnerId driverId vehicleNo req

getDriverFleetTripTransactions :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Text -> Maybe Int -> Maybe Int -> Flow Common.TripTransactionResp
getDriverFleetTripTransactions merchantShortId opCity apiTokenInfo driverId vehicleNo mbLimit mbOffset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetTripTransactions) fleetOwnerId driverId vehicleNo mbLimit mbOffset

postDriverFleetAddDrivers :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.CreateDriversReq -> Flow APISuccess
postDriverFleetAddDrivers merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  T.withTransactionStoring transaction $ Client.callFleetAPI checkedMerchantId opCity (Common.addMultipartBoundary "XXX00XXX" . (.driverDSL.postDriverFleetAddDrivers)) req{fleetOwnerId = Just fleetOwnerId}

postDriverFleetAddDriverBusRouteMapping :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.CreateDriverBusRouteMappingReq -> Flow APISuccess
postDriverFleetAddDriverBusRouteMapping merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  T.withTransactionStoring transaction $ Client.callFleetAPI checkedMerchantId opCity (Common.addMultipartBoundary "XXX00XXX" . (.driverDSL.postDriverFleetAddDriverBusRouteMapping)) req{fleetOwnerId = Just fleetOwnerId}

getDriverDashboardFleetTrackDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.TrackDriverLocationsReq -> Flow Common.TrackDriverLocationsRes
getDriverDashboardFleetTrackDriver merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverDashboardFleetTrackDriver) fleetOwnerId req
