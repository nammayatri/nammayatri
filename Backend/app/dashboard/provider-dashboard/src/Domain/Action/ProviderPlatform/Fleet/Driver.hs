{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
    postDriverDashboardFleetWmbTripEnd,
    postDriverFleetAddDrivers,
    getDriverFleetRoutes,
    getDriverFleetPossibleRoutes,
    postDriverFleetTripPlanner,
    getDriverFleetTripTransactions,
    postDriverFleetAddDriverBusRouteMapping,
    postDriverDashboardFleetTrackDriver,
    getDriverFleetWmbRouteDetails,
    postDriverFleetGetNearbyDrivers,
  )
where

import qualified API.Client.ProviderPlatform.Fleet as Client
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as Common
import qualified "dashboard-helper-api" Dashboard.Common as DCommon
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Management.DriverRegistration as Registration
import Data.Text
import "lib-dashboard" Domain.Action.Dashboard.Person as DPerson
import Domain.Types.FleetControlGroup
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

postDriverFleetAddVehicle :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> Maybe Text -> Maybe Text -> Common.AddVehicleReq -> Flow APISuccess
postDriverFleetAddVehicle merchantShortId opCity apiTokenInfo phoneNo mbMobileCountryCode fleetControlGroupId req = do
  runRequestValidation Common.validateAddVehicleReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetAddVehicle) phoneNo fleetOwnerId mbMobileCountryCode fleetControlGroupId req

postDriverFleetAddRCWithoutDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Registration.RegisterRCReq -> Flow APISuccess
postDriverFleetAddRCWithoutDriver merchantShortId opCity apiTokenInfo fleetControlGroupId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetAddRCWithoutDriver) fleetOwnerId fleetControlGroupId req

getDriverFleetGetAllVehicle :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe (Id Registration.Person) -> Maybe [Text] -> Flow Common.ListVehicleRes
getDriverFleetGetAllVehicle merchantShortId opCity apiTokenInfo mbLimit mbOffset mbRegNumberString fleetMemberId fleetControlGroupList = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetGetAllVehicle) fleetOwnerId mbLimit mbOffset mbRegNumberString fleetMemberId fleetControlGroupList

getDriverFleetGetAllDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe (Id Registration.Person) -> Maybe [Text] -> Flow Common.FleetListDriverRes
getDriverFleetGetAllDriver merchantShortId opCity apiTokenInfo mbLimit mbOffset mbMobileNumber mbName mbSearchString fleetMemberId fleetControlGroupList = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetGetAllDriver) fleetOwnerId mbLimit mbOffset mbMobileNumber mbName mbSearchString fleetMemberId fleetControlGroupList

postDriverFleetUnlink :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Text -> Maybe Text -> Flow APISuccess
postDriverFleetUnlink merchantShortId opCity apiTokenInfo driverId vehicleNo fleetControlGroupId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetUnlink) fleetOwnerId driverId vehicleNo fleetControlGroupId

postDriverFleetRemoveVehicle :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> Maybe Text -> Flow APISuccess
postDriverFleetRemoveVehicle merchantShortId opCity apiTokenInfo vehicleNo fleetControlGroupId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetRemoveVehicle) fleetOwnerId vehicleNo fleetControlGroupId

postDriverFleetRemoveDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Maybe Text -> Flow APISuccess
postDriverFleetRemoveDriver merchantShortId opCity apiTokenInfo driverId fleetControlGroupId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetRemoveDriver) fleetOwnerId driverId fleetControlGroupId

getDriverFleetTotalEarning :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe UTCTime -> Maybe UTCTime -> Maybe (Id Registration.Person) -> Maybe [Text] -> Flow Common.FleetTotalEarningResponse
getDriverFleetTotalEarning merchantShortId opCity apiTokenInfo mbFrom mbTo fleetMemberId fleetControlGroupList = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetTotalEarning) fleetOwnerId mbFrom mbTo fleetMemberId fleetControlGroupList

getDriverFleetVehicleEarning :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe UTCTime -> Maybe UTCTime -> Maybe (Id Registration.Person) -> Maybe [Text] -> Flow Common.FleetEarningListRes
getDriverFleetVehicleEarning merchantShortId opCity apiTokenInfo mbVehicleNo mbLimit mbOffset mbFrom mbTo fleetMemberId fleetControlGroupList = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetVehicleEarning) fleetOwnerId mbVehicleNo mbLimit mbOffset mbFrom mbTo fleetMemberId fleetControlGroupList

getDriverFleetDriverEarning :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe UTCTime -> Maybe UTCTime -> Maybe Bool -> Maybe Common.SortOn -> Maybe (Id Registration.Person) -> Maybe [Text] -> Flow Common.FleetEarningListRes
getDriverFleetDriverEarning merchantShortId opCity apiTokenInfo mbMobileCountryCode mbMobileNo mbLimit mbOffset mbFrom mbTo mbSortDesc mbSortOn fleetMemberId fleetControlGroupList = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetDriverEarning) fleetOwnerId mbMobileCountryCode mbMobileNo mbLimit mbOffset mbFrom mbTo mbSortDesc mbSortOn fleetMemberId fleetControlGroupList

getDriverFleetDriverVehicleAssociation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Maybe (Id Registration.Person) -> Maybe [Text] -> Flow Common.DrivertoVehicleAssociationRes
getDriverFleetDriverVehicleAssociation merchantId opCity apiTokenInfo mbLimit mbOffset mbCountryCode mbPhoneNo mbVehicleNo mbStatus mbFrom mbTo fleetMemberId fleetControlGroupList = do
  checkedMerchantId <- merchantCityAccessCheck merchantId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetDriverVehicleAssociation) fleetOwnerId mbLimit mbOffset mbCountryCode mbPhoneNo mbVehicleNo mbStatus mbFrom mbTo fleetMemberId fleetControlGroupList

getDriverFleetDriverAssociation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Maybe Common.DriverMode -> Maybe Text -> Maybe Text -> Maybe (Id Registration.Person) -> Maybe [Text] -> Flow Common.DrivertoVehicleAssociationRes
getDriverFleetDriverAssociation merhcantId opCity apiTokenInfo mbIsActive mbLimit mbOffset mbCountryCode mbPhoneNo mbStats mbFrom mbTo mbMode name mbSearchString fleetMemberId fleetControlGroupList = do
  checkedMerchantId <- merchantCityAccessCheck merhcantId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetDriverAssociation) fleetOwnerId mbIsActive mbLimit mbOffset mbCountryCode mbPhoneNo mbStats mbFrom mbTo mbMode name mbSearchString fleetMemberId fleetControlGroupList

getDriverFleetVehicleAssociation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Maybe Common.FleetVehicleStatus -> Maybe (Id Registration.Person) -> Maybe [Text] -> Flow Common.DrivertoVehicleAssociationRes
getDriverFleetVehicleAssociation merhcantId opCity apiTokenInfo mbLimit mbOffset mbVehicleNo mbIncludeStats mbFrom mbTo mbStatus fleetMemberId fleetControlGroupList = do
  checkedMerchantId <- merchantCityAccessCheck merhcantId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetVehicleAssociation) fleetOwnerId mbLimit mbOffset mbVehicleNo mbIncludeStats mbFrom mbTo mbStatus fleetMemberId fleetControlGroupList

postDriverFleetVehicleDriverRcStatus :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Maybe Text -> Common.RCStatusReq -> Flow APISuccess
postDriverFleetVehicleDriverRcStatus merchantShortId opCity apiTokenInfo driverId fleetControlGroupId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) $ Just req
  T.withTransactionStoring transaction $ do
    fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
    Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetVehicleDriverRcStatus) driverId fleetOwnerId fleetControlGroupId req

postDriverUpdateFleetOwnerInfo :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Maybe Text -> Common.UpdateFleetOwnerInfoReq -> Flow APISuccess
postDriverUpdateFleetOwnerInfo merchantShortId opCity apiTokenInfo driverId fleetControlGroupId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Just req)
  T.withTransactionStoring transaction $ do
    fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
    unless (apiTokenInfo.personId.getId == driverId.getId && fleetOwnerId == apiTokenInfo.personId.getId) $
      throwError AccessDenied
    _ <- Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverUpdateFleetOwnerInfo) (Id fleetOwnerId) fleetControlGroupId req
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

getDriverFleetOwnerInfo :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Maybe (Id Registration.Person) -> Maybe [Text] -> Flow Common.FleetOwnerInfoRes
getDriverFleetOwnerInfo merchantShortId opCity apiTokenInfo driverId fleetMemberId fleetControlGroupList = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetOwnerInfo) driverId fleetMemberId fleetControlGroupList

postDriverFleetSendJoiningOtp :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Registration.AuthReq -> Flow Registration.AuthRes
postDriverFleetSendJoiningOtp merchantShortId opCity apiTokenInfo fleetControlGroupId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  person <- QP.findById apiTokenInfo.personId >>= fromMaybeM (PersonNotFound fleetOwnerId)
  let dashboardUserName = person.firstName <> " " <> person.lastName
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetSendJoiningOtp) dashboardUserName fleetControlGroupId req

postDriverFleetVerifyJoiningOtp :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Maybe Text -> Common.VerifyFleetJoiningOtpReq -> Flow APISuccess
postDriverFleetVerifyJoiningOtp merchantShortId opCity apiTokenInfo mbAuthId fleetControlGroupId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetVerifyJoiningOtp) fleetOwnerId mbAuthId fleetControlGroupId req

postDriverFleetLinkRCWithDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Common.LinkRCWithDriverForFleetReq -> Flow APISuccess
postDriverFleetLinkRCWithDriver merchantShortId opCity apiTokenInfo fleetControlGroupId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetLinkRCWithDriver) fleetOwnerId fleetControlGroupId req

postDriverFleetAddVehicles :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Common.CreateVehiclesReq -> Flow Common.APISuccessWithUnprocessedEntities
postDriverFleetAddVehicles merchantShortId opCity apiTokenInfo fleetControlGroupId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  T.withTransactionStoring transaction $
    Client.callFleetAPI
      checkedMerchantId
      opCity
      (Common.addMultipartBoundary "XXX00XXX" Client.postDriverFleetAddVehicles)
      (req{fleetOwnerId = Just fleetOwnerId})

getDriverFleetGetDriverRequests :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe UTCTime -> Maybe UTCTime -> Maybe Common.RequestStatus -> Maybe Int -> Maybe Int -> Maybe (Id Registration.Person) -> Maybe [Text] -> Flow Common.DriverRequestResp
getDriverFleetGetDriverRequests merchantShortId opCity apiTokenInfo mbFrom mbTo mbStatus mbLimit mbOffset fleetMemberAssociation fleetControlGroupList = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetGetDriverRequests) fleetOwnerId mbFrom mbTo mbStatus mbLimit mbOffset fleetMemberAssociation fleetControlGroupList

postDriverFleetRespondDriverRequest :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Common.RequestRespondReq -> Flow APISuccess
postDriverFleetRespondDriverRequest merchantShortId opCity apiTokenInfo fleetControlGroupId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing $ Just req
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  T.withTransactionStoring transaction $
    Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetRespondDriverRequest) fleetOwnerId fleetControlGroupId req

postDriverDashboardFleetWmbTripEnd :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.TripTransaction -> Maybe Text -> Flow APISuccess
postDriverDashboardFleetWmbTripEnd merchantShortId opCity apiTokenInfo tripTransactionId fleetControlGroupId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just $ DCommon.TransactionLogId tripTransactionId.getId)
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  T.withTransactionStoring transaction $ Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverDashboardFleetWmbTripEnd) tripTransactionId fleetOwnerId fleetControlGroupId

getDriverFleetRoutes :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe LatLong -> Maybe Text -> Maybe (Id Registration.Person) -> Maybe [Text] -> Int -> Int -> Flow Common.RouteAPIResp
getDriverFleetRoutes merchantShortId opCity apiTokenInfo mbCurrentLocation mbSearchString fleetMemberId fleetControlGroupList limit offset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetRoutes) fleetOwnerId mbCurrentLocation mbSearchString fleetMemberId fleetControlGroupList limit offset

getDriverFleetPossibleRoutes :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe (Id Registration.Person) -> Maybe [Text] -> Text -> Flow Common.RouteAPIResp
getDriverFleetPossibleRoutes merchantShortId opCity apiTokenInfo fleetMemberId fleetControlGroupList startStopCode = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetPossibleRoutes) fleetOwnerId fleetMemberId fleetControlGroupList startStopCode

postDriverFleetTripPlanner :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Common.TripPlannerReq -> Flow APISuccess
postDriverFleetTripPlanner merchantShortId opCity apiTokenInfo fleetControlGroupId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  T.withTransactionStoring transaction $ Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetTripPlanner) fleetOwnerId fleetControlGroupId req

getDriverFleetTripTransactions :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe (Id Registration.Person) -> Maybe [Text] -> Int -> Int -> Flow Common.TripTransactionResp
getDriverFleetTripTransactions merchantShortId opCity apiTokenInfo driverId mbFrom mbTo mbVehicleNumber fleetMemberId fleetControlGroupList limit offset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetTripTransactions) fleetOwnerId driverId mbFrom mbTo mbVehicleNumber fleetMemberId fleetControlGroupList limit offset

postDriverFleetAddDrivers :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Common.CreateDriversReq -> Flow Common.APISuccessWithUnprocessedEntities
postDriverFleetAddDrivers merchantShortId opCity apiTokenInfo fleetControlGroupId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  T.withTransactionStoring transaction $
    Client.callFleetAPI
      checkedMerchantId
      opCity
      (Common.addMultipartBoundary "XXX00XXX" (.driverDSL.postDriverFleetAddDrivers))
      (req{fleetOwnerId = Just fleetOwnerId})

postDriverFleetAddDriverBusRouteMapping :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Common.CreateDriverBusRouteMappingReq -> Flow Common.APISuccessWithUnprocessedEntities
postDriverFleetAddDriverBusRouteMapping merchantShortId opCity apiTokenInfo fleetControlGroupId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  T.withTransactionStoring transaction $
    Client.callFleetAPI
      checkedMerchantId
      opCity
      (Common.addMultipartBoundary "XXX00XXX" Client.postDriverFleetAddDriverBusRouteMapping)
      (req{fleetOwnerId = Just fleetOwnerId})

postDriverDashboardFleetTrackDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe (Id Registration.Person) -> Maybe [Text] -> Common.TrackDriverLocationsReq -> Flow Common.TrackDriverLocationsRes
postDriverDashboardFleetTrackDriver merchantShortId opCity apiTokenInfo fleetMemberId fleetControlGroupList req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverDashboardFleetTrackDriver) fleetOwnerId fleetMemberId fleetControlGroupList req

getDriverFleetWmbRouteDetails :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> Maybe (Id Registration.Person) -> Maybe [Text] -> Flow Common.RouteDetails
getDriverFleetWmbRouteDetails merchantShortId opCity apiTokenInfo routeCode fleetMemberId fleetControlGroupList = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetWmbRouteDetails) fleetOwnerId routeCode fleetMemberId fleetControlGroupList

postDriverFleetGetNearbyDrivers :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe (Id Registration.Person) -> Maybe [Text] -> Common.NearbyDriverReq -> Flow Common.NearbyDriverResp
postDriverFleetGetNearbyDrivers merchantShortId opCity apiTokenInfo fleetMemberId fleetControlGroupList req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId
  T.withTransactionStoring transaction $ Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetGetNearbyDrivers) fleetOwnerId fleetMemberId fleetControlGroupList req
