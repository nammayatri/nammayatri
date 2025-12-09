{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.ProviderPlatform.Fleet.Driver
  ( getDriverFleetGetAllBadge,
    getDriverFleetAccessList,
    postDriverFleetAccessSelect,
    postDriverFleetV2AccessSelect,
    postDriverFleetAddVehicle,
    postDriverFleetAddRCWithoutDriver,
    getDriverFleetGetAllVehicle,
    getDriverFleetGetAllDriver,
    postDriverFleetUnlink,
    postDriverFleetRemoveVehicle,
    getFleetOwnerId,
    getFleetOwnerIds,
    verifyFleetOwnerAccess,
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
    getDriverDashboardInternalHelperGetFleetOwnerId,
    getDriverDashboardInternalHelperGetFleetOwnerIds,
    postDriverFleetV2AccessMultiOwnerIdSelect,
    getDriverFleetStatus,
    getMbFleetOwnerAndRequestorIdMerchantBased,
    getDriverFleetBookings,
    getDriverFleetAssignments,
    getDriverFleetOperatorInfo,
    postDriverFleetLocationList,
    postDriverFleetGetDriverDetails,
    postDriverFleetGetNearbyDriversV2,
    getDriverFleetDashboardAnalyticsAllTime,
    getDriverFleetDashboardAnalytics,
    getDriverDashboardFleetTripWaypoints,
    postDriverDashboardFleetEstimateRoute,
    postDriverFleetTripTransactionsV2,
    postDriverFleetDriverUpdate,
    postDriverFleetApproveDriver,
    getDriverFleetDriverListStats,
    getDriverFleetVehicleListStats,
    getDriverFleetDriverOnboardedDriversAndUnlinkedVehicles,
    getDriverFleetDriverDetails,
  )
where

import qualified API.Client.ProviderPlatform.Fleet as Client
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as Common
import qualified "dashboard-helper-api" Dashboard.Common as DCommon
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Management.DriverRegistration as Registration
import qualified Data.ByteString.Lazy as LBS
import Data.Text hiding (elem, filter, find, length, map, null)
import Data.Time (Day)
import qualified Domain.Action.Dashboard.Common as DCommon
import "lib-dashboard" Domain.Action.Dashboard.Person as DPerson
import Domain.Action.ProviderPlatform.CheckVerification (checkFleetOwnerVerification)
import Domain.Types.Alert
import Domain.Types.Alert.AlertRequestStatus ()
import Domain.Types.FleetBadgeType as DFBT
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified "lib-dashboard" Domain.Types.Person as DP
import qualified "lib-dashboard" Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import EulerHS.Prelude hiding (elem, find, length, map, null, sortOn, whenJust)
import qualified Kernel.External.Maps
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation (runRequestValidation)
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
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

-----------------------------------------------------------------------------------------------------------------------
----------------------------------------------- ACCESS CONTROL LAYER --------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

--------------------------------------- Single Fleet Owners Access --------------------------------------
getFleetOwnerId :: Text -> Maybe Text -> Flow Text
getFleetOwnerId memberPersonId mbFleetOwnerId = Client.callFleetAPI (skipMerchantCityAccessCheck (ShortId "no_required")) City.AnyCity (.driverDSL.getDriverDashboardInternalHelperGetFleetOwnerId) mbFleetOwnerId memberPersonId

------------------------------------- Helper Functions --------------------------------------
-- in case if fleet owner is mandatory
getFleetOwnerAndRequestorIdMerchantBased :: ApiTokenInfo -> Maybe Text -> Flow (Text, Text)
getFleetOwnerAndRequestorIdMerchantBased apiTokenInfo mbFleetOwnerId = do
  (mbFleetOwnerId', requestorId) <- getMbFleetOwnerAndRequestorIdMerchantBased apiTokenInfo mbFleetOwnerId
  fleetOwnerId <- mbFleetOwnerId' & fromMaybeM (InvalidRequest "fleetOwnerId required")
  return (fleetOwnerId, requestorId)

-- in case if fleet owner is optional
getMbFleetOwnerAndRequestorIdMerchantBased :: ApiTokenInfo -> Maybe Text -> Flow (Maybe Text, Text)
getMbFleetOwnerAndRequestorIdMerchantBased apiTokenInfo mbFleetOwnerId = do
  case apiTokenInfo.merchant.hasFleetMemberHierarchy of
    Just False -> do
      -- MSIL: requestor is fleet owner or operator, access check on bpp side required!
      let requestorId = apiTokenInfo.personId.getId
      if DP.isFleetOwner apiTokenInfo.person
        then do
          -- requestor is fleet owner
          whenJust mbFleetOwnerId $ \fleetOwnerId ->
            unless (fleetOwnerId == requestorId) $ throwError AccessDenied
          return (Just requestorId, requestorId)
        else do
          -- requestor is operator
          return (mbFleetOwnerId, requestorId)
    _ -> do
      -- Existing flow: consider requestor the same as fleet owner, fleet member operates on befalf of fleet owner
      fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId mbFleetOwnerId
      return (Just fleetOwnerId, fleetOwnerId)

--- moved to driver side ---
-- data FleetOwnerInfo = FleetOwnerInfo
--   { fleetOwnerId :: Text,
--     fleetOwnerName :: Text,
--     requestorId :: Text
--   }

-- getFleetOwnersInfoMerchantBased :: ApiTokenInfo -> Maybe Text -> Flow [FleetOwnerInfo]
-- getFleetOwnersInfoMerchantBased apiTokenInfo mbFleetOwnerId = do
--   case apiTokenInfo.merchant.hasFleetMemberHierarchy of
--     Just False -> do
--       -- MSIL: requestor is fleet owner or operator, access check on bpp side required!
--       let requestor = apiTokenInfo.person
--           requestorId = requestor.id.getId
--       if apiTokenInfo.person.dashboardAccessType `elem` [Just DRole.FLEET_OWNER, Just DRole.RENTAL_FLEET_OWNER]
--         then do
--           -- requestor is fleet owner
--           whenJust mbFleetOwnerId $ \fleetOwnerId ->
--             unless (fleetOwnerId == requestorId) $ throwError AccessDenied
--           let fleetOwnerName = requestor.firstName <> " " <> requestor.lastName
--           return [FleetOwnerInfo {fleetOwnerId = requestorId, fleetOwnerName, requestorId}]
--         else do
--           -- requestor is operator
--           fleetOwnerId <- mbFleetOwnerId & fromMaybeM (InvalidRequest "fleetOwnerId required")
--           fleetOwner <- QP.findById (Id fleetOwnerId) >>= fromMaybeM (PersonNotFound fleetOwnerId)
--           let fleetOwnerName = fleetOwner.firstName <> " " <> fleetOwner.lastName
--           return [FleetOwnerInfo {fleetOwnerId, fleetOwnerName, requestorId}]
--     _ -> do
--       -- Existing flow: consider requestor the same as fleet owner, fleet member operates on befalf of fleet owner
--       fleetOwnerIds <- getFleetOwnerIds apiTokenInfo.personId.getId mbFleetOwnerId
--       return $ (\(fleetOwnerId, fleetOwnerName) -> FleetOwnerInfo {fleetOwnerId, fleetOwnerName, requestorId = fleetOwnerId}) <$> fleetOwnerIds

------------------------------------- Multiple Fleet Owners Access --------------------------------------
getFleetOwnerIds :: Text -> Maybe Text -> Flow [(Text, Text)]
getFleetOwnerIds memberPersonId mbFleetOwnerId = do
  mbCachedFleetOwnerIds :: Maybe [(Text, Text)] <- Redis.get redisKey
  case mbCachedFleetOwnerIds of
    Just fleetOwnerIds -> return fleetOwnerIds
    Nothing -> do
      fleetOwnerIds <- Client.callFleetAPI (skipMerchantCityAccessCheck (ShortId "no_required")) City.AnyCity (.driverDSL.getDriverDashboardInternalHelperGetFleetOwnerIds) mbFleetOwnerId memberPersonId
      Redis.setExp redisKey fleetOwnerIds 180
      return fleetOwnerIds
  where
    redisKey = "CachedFleetOwnerIds:memberPersonId-" <> memberPersonId <> "-fleetOwnerId-" <> fromMaybe "None" mbFleetOwnerId

------------------------------------- Verify Fleet Owners Access --------------------------------------
verifyFleetOwnerAccess :: Text -> Text -> Flow Text
verifyFleetOwnerAccess fleetMemberId accessedFleetOwnerId = do
  fleetOwnerIds <- getFleetOwnerIds fleetMemberId Nothing
  (fleetOwnerId, _) <- find (\(fleetOwnerId, _) -> fleetOwnerId == accessedFleetOwnerId) fleetOwnerIds & fromMaybeM AccessDenied
  return fleetOwnerId

------------------------------------- Fleet Owners Access Control --------------------------------------
postDriverFleetAccessSelect :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> Maybe Text -> Maybe Bool -> Bool -> Flow APISuccess
postDriverFleetAccessSelect merchantShortId opCity apiTokenInfo fleetOwnerId _ mbOnlySingle enable = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let fleetMemberId = apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetAccessSelect) fleetOwnerId (Just fleetMemberId) mbOnlySingle enable

postDriverFleetV2AccessSelect :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> Bool -> Flow APISuccess
postDriverFleetV2AccessSelect merchantShortId opCity apiTokenInfo _ mbFleetOwnerId mbGroupCode mbOnlyCurrent enable = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let fleetMemberId = apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetV2AccessSelect) (Just fleetMemberId) mbFleetOwnerId mbGroupCode mbOnlyCurrent enable

------------------------------------- Fleet Owners Access List --------------------------------------
getDriverFleetAccessList :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Flow Common.FleetOwnerListRes
getDriverFleetAccessList merchantShortId opCity apiTokenInfo _ = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let fleetMemberId = apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetAccessList) (Just fleetMemberId)

-------------------------------------------------------------------------------------------------------------
----------------------------------- WRITE LAYER (Single Fleet Level) ---------------------------------
-------------------------------------------------------------------------------------------------------------

postDriverFleetAddVehicle :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> Maybe Text -> Maybe Text -> Maybe Common.Role -> Common.AddVehicleReq -> Flow APISuccess
postDriverFleetAddVehicle merchantShortId opCity apiTokenInfo phoneNo mbMobileCountryCode mbFleetOwnerId mbRole req = do
  checkFleetOwnerVerification apiTokenInfo.personId
  runRequestValidation Common.validateAddVehicleReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  (mbFleetOwnerId', requestorId) <- getMbFleetOwnerAndRequestorIdMerchantBased apiTokenInfo mbFleetOwnerId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetAddVehicle) phoneNo requestorId mbFleetOwnerId' mbMobileCountryCode mbRole req -- apiTokenInfo may contain opertaor or fleet

postDriverFleetAddRCWithoutDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Registration.RegisterRCReq -> Flow APISuccess
postDriverFleetAddRCWithoutDriver merchantShortId opCity apiTokenInfo mbFleetOwnerId req = do
  checkFleetOwnerVerification apiTokenInfo.personId
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId mbFleetOwnerId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetAddRCWithoutDriver) fleetOwnerId req

postDriverFleetVehicleDriverRcStatus :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Maybe Text -> Common.RCStatusReq -> Flow APISuccess
postDriverFleetVehicleDriverRcStatus merchantShortId opCity apiTokenInfo driverId mbFleetOwnerId req = do
  checkFleetOwnerVerification apiTokenInfo.personId
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) $ Just req
  T.withTransactionStoring transaction $ do
    (mbFleetOwnerId', requestorId) <- getMbFleetOwnerAndRequestorIdMerchantBased apiTokenInfo mbFleetOwnerId
    Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetVehicleDriverRcStatus) driverId requestorId mbFleetOwnerId' req

postDriverUpdateFleetOwnerInfo :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Maybe Text -> Common.UpdateFleetOwnerInfoReq -> Flow APISuccess
postDriverUpdateFleetOwnerInfo merchantShortId opCity apiTokenInfo driverId mbFleetOwnerId req = do
  runRequestValidation Common.validateUpdateFleetOwnerInfoReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Just req)
  T.withTransactionStoring transaction $ do
    fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId mbFleetOwnerId
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

postDriverFleetSendJoiningOtp :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Registration.AuthReq -> Flow Registration.AuthRes
postDriverFleetSendJoiningOtp merchantShortId opCity apiTokenInfo mbFleetOwnerId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  (fleetOwnerId, requestorId) <- getFleetOwnerAndRequestorIdMerchantBased apiTokenInfo mbFleetOwnerId
  person <- QP.findById apiTokenInfo.personId >>= fromMaybeM (PersonNotFound fleetOwnerId)
  let dashboardUserName = person.firstName <> " " <> person.lastName
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetSendJoiningOtp) dashboardUserName (Just fleetOwnerId) (Just requestorId) req

postDriverFleetVerifyJoiningOtp :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Maybe Text -> Common.VerifyFleetJoiningOtpReq -> Flow APISuccess
postDriverFleetVerifyJoiningOtp merchantShortId opCity apiTokenInfo mbAuthId mbFleetOwnerId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  (fleetOwnerId, requestorId) <- getFleetOwnerAndRequestorIdMerchantBased apiTokenInfo mbFleetOwnerId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetVerifyJoiningOtp) fleetOwnerId mbAuthId (Just requestorId) req

postDriverFleetLinkRCWithDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Common.LinkRCWithDriverForFleetReq -> Flow APISuccess
postDriverFleetLinkRCWithDriver merchantShortId opCity apiTokenInfo mbFleetOwnerId req = do
  checkFleetOwnerVerification apiTokenInfo.personId
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  (fleetOwnerId, requestorId) <- getFleetOwnerAndRequestorIdMerchantBased apiTokenInfo mbFleetOwnerId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetLinkRCWithDriver) fleetOwnerId (Just requestorId) req

postDriverFleetAddVehicles :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Common.CreateVehiclesReq -> Flow Common.APISuccessWithUnprocessedEntities
postDriverFleetAddVehicles merchantShortId opCity apiTokenInfo mbFleetOwnerId req = do
  checkFleetOwnerVerification apiTokenInfo.personId
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  (mbFleetOwnerId', requestorId) <- getMbFleetOwnerAndRequestorIdMerchantBased apiTokenInfo mbFleetOwnerId
  T.withTransactionStoring transaction $ Client.callFleetAPI checkedMerchantId opCity (Common.addMultipartBoundary "XXX00XXX" . (.driverDSL.postDriverFleetAddVehicles)) req{fleetOwnerId = mbFleetOwnerId', requestorId = Just requestorId}

postDriverFleetTripPlanner :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Common.TripPlannerReq -> Flow APISuccess
postDriverFleetTripPlanner merchantShortId opCity apiTokenInfo mbFleetOwnerId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId mbFleetOwnerId
  T.withTransactionStoring transaction $ Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetTripPlanner) fleetOwnerId req

postDriverFleetAddDrivers :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Common.CreateDriversReq -> Flow Common.APISuccessWithUnprocessedEntities
postDriverFleetAddDrivers merchantShortId opCity apiTokenInfo mbFleetOwnerId req = do
  checkFleetOwnerVerification apiTokenInfo.personId
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  -- for MSIL flow fleetOwner is in csv row, so put fleetOwner in request is not mandatory
  (mbFleetOwnerId', requestorId) <- getMbFleetOwnerAndRequestorIdMerchantBased apiTokenInfo mbFleetOwnerId
  T.withTransactionStoring transaction $ Client.callFleetAPI checkedMerchantId opCity (addMultipartBoundary "XXX00XXX" . (.driverDSL.postDriverFleetAddDrivers)) (Just requestorId) req{fleetOwnerId = mbFleetOwnerId'}
  where
    addMultipartBoundary :: LBS.ByteString -> (Maybe Text -> (LBS.ByteString, req) -> res) -> Maybe Text -> req -> res
    addMultipartBoundary boundary clientFn requestorId reqBody = clientFn requestorId (boundary, reqBody)

postDriverFleetAddDriverBusRouteMapping :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Common.CreateDriverBusRouteMappingReq -> Flow Common.APISuccessWithUnprocessedEntities
postDriverFleetAddDriverBusRouteMapping merchantShortId opCity apiTokenInfo mbFleetOwnerId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId mbFleetOwnerId
  T.withTransactionStoring transaction $ Client.callFleetAPI checkedMerchantId opCity (Common.addMultipartBoundary "XXX00XXX" . (.driverDSL.postDriverFleetAddDriverBusRouteMapping)) req{fleetOwnerId = Just fleetOwnerId}

postDriverFleetRespondDriverRequest :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Common.RequestRespondReq -> Flow APISuccess
postDriverFleetRespondDriverRequest merchantShortId opCity apiTokenInfo mbFleetOwnerId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing $ Just req
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId mbFleetOwnerId
  T.withTransactionStoring transaction $
    Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetRespondDriverRequest) fleetOwnerId req

postDriverDashboardFleetWmbTripEnd :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.TripTransaction -> Maybe Text -> Maybe Common.ActionSource -> Flow APISuccess
postDriverDashboardFleetWmbTripEnd merchantShortId opCity apiTokenInfo tripTransactionId mbFleetOwnerId mbTerminationSource = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just $ DCommon.TransactionLogId tripTransactionId.getId)
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId mbFleetOwnerId
  T.withTransactionStoring transaction $ Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverDashboardFleetWmbTripEnd) tripTransactionId fleetOwnerId mbTerminationSource

postDriverFleetUnlink :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Text -> Maybe Text -> Flow APISuccess
postDriverFleetUnlink merchantShortId opCity apiTokenInfo driverId vehicleNo mbFleetOwnerId = do
  checkFleetOwnerVerification apiTokenInfo.personId
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  (mbFleetOwnerId', requestorId) <- getMbFleetOwnerAndRequestorIdMerchantBased apiTokenInfo mbFleetOwnerId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetUnlink) requestorId driverId vehicleNo mbFleetOwnerId'

postDriverFleetRemoveVehicle :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> Maybe Text -> Flow APISuccess
postDriverFleetRemoveVehicle merchantShortId opCity apiTokenInfo vehicleNo mbFleetOwnerId = do
  checkFleetOwnerVerification apiTokenInfo.personId
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  (fleetOwnerId, requestorId) <- getFleetOwnerAndRequestorIdMerchantBased apiTokenInfo mbFleetOwnerId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetRemoveVehicle) fleetOwnerId vehicleNo (Just requestorId)

postDriverFleetRemoveDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Maybe Text -> Flow APISuccess
postDriverFleetRemoveDriver merchantShortId opCity apiTokenInfo driverId mbFleetOwnerId = do
  checkFleetOwnerVerification apiTokenInfo.personId
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  (mbFleetOwnerId', requestorId) <- getMbFleetOwnerAndRequestorIdMerchantBased apiTokenInfo mbFleetOwnerId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetRemoveDriver) requestorId driverId mbFleetOwnerId'

---------------------------------------------------------------------------------------------------------------
---------------------------------------- READ LAYER (Single Fleet Level) --------------------------------------
---------------------------------------------------------------------------------------------------------------

getDriverFleetTotalEarning :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe UTCTime -> Maybe UTCTime -> Flow Common.FleetTotalEarningResponse
getDriverFleetTotalEarning merchantShortId opCity apiTokenInfo mbFrom mbTo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId Nothing
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetTotalEarning) fleetOwnerId mbFrom mbTo

getDriverFleetVehicleEarning :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe UTCTime -> Maybe UTCTime -> Flow Common.FleetEarningListRes
getDriverFleetVehicleEarning merchantShortId opCity apiTokenInfo mbVehicleNo mbLimit mbOffset mbFrom mbTo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId Nothing
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetVehicleEarning) fleetOwnerId mbVehicleNo mbLimit mbOffset mbFrom mbTo

getDriverFleetDriverEarning :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe UTCTime -> Maybe UTCTime -> Maybe Bool -> Maybe Common.SortOn -> Flow Common.FleetEarningListRes
getDriverFleetDriverEarning merchantShortId opCity apiTokenInfo mbMobileCountryCode mbMobileNo mbLimit mbOffset mbFrom mbTo mbSortDesc mbSortOn = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId Nothing
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetDriverEarning) fleetOwnerId mbMobileCountryCode mbMobileNo mbLimit mbOffset mbFrom mbTo mbSortDesc mbSortOn

getDriverFleetDriverVehicleAssociation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Flow Common.DrivertoVehicleAssociationRes
getDriverFleetDriverVehicleAssociation merchantId opCity apiTokenInfo mbLimit mbOffset mbCountryCode mbPhoneNo mbVehicleNo mbStatus mbFrom mbTo = do
  checkFleetOwnerVerification apiTokenInfo.personId
  checkedMerchantId <- merchantCityAccessCheck merchantId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId Nothing
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetDriverVehicleAssociation) fleetOwnerId mbLimit mbOffset mbCountryCode mbPhoneNo mbVehicleNo mbStatus mbFrom mbTo

getDriverFleetRoutes :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe LatLong -> Maybe Text -> Maybe Text -> Int -> Int -> Flow Common.RouteAPIResp
getDriverFleetRoutes merchantShortId opCity apiTokenInfo mbCurrentLocation mbSearchString mbFleetOwnerId limit offset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId mbFleetOwnerId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetRoutes) fleetOwnerId mbCurrentLocation mbSearchString limit offset

getDriverFleetPossibleRoutes :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Text -> Flow Common.RouteAPIResp
getDriverFleetPossibleRoutes merchantShortId opCity apiTokenInfo mbFleetOwnerId startStopCode = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId mbFleetOwnerId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetPossibleRoutes) fleetOwnerId startStopCode

getDriverFleetWmbRouteDetails :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> Maybe Text -> Flow Common.RouteDetails
getDriverFleetWmbRouteDetails merchantShortId opCity apiTokenInfo routeCode mbFleetOwnerId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId mbFleetOwnerId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetWmbRouteDetails) fleetOwnerId routeCode

postDriverDashboardFleetTrackDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Common.TrackDriverLocationsReq -> Flow Common.TrackDriverLocationsRes
postDriverDashboardFleetTrackDriver merchantShortId opCity apiTokenInfo mbFleetOwnerId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId mbFleetOwnerId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverDashboardFleetTrackDriver) fleetOwnerId req

getDriverFleetOwnerInfo :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Flow Common.FleetOwnerInfoRes
getDriverFleetOwnerInfo merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetOwnerInfo) driverId

getDriverFleetStatus :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Flow Common.DriverStatusRes
getDriverFleetStatus merchantShortId opCity apiTokenInfo mbFleetOwnerId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  (mbFleetOwnerId', requestorId) <- getMbFleetOwnerAndRequestorIdMerchantBased apiTokenInfo mbFleetOwnerId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetStatus) requestorId mbFleetOwnerId'

getDriverFleetDashboardAnalyticsAllTime :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Flow Common.AllTimeFleetAnalyticsRes
getDriverFleetDashboardAnalyticsAllTime merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId Nothing
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetDashboardAnalyticsAllTime) fleetOwnerId

getDriverFleetDashboardAnalytics :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Common.FleetAnalyticsResponseType -> Day -> Day -> Flow Common.FleetAnalyticsRes
getDriverFleetDashboardAnalytics merchantShortId opCity apiTokenInfo mbResponseType from to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId Nothing
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetDashboardAnalytics) fleetOwnerId mbResponseType from to

getDriverFleetDriverListStats :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Day -> Maybe Day -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Common.FleetDriverListStatsSortOn -> Maybe Common.FleetDriverStatsResponseType -> Flow Common.FleetDriverStatsListRes
getDriverFleetDriverListStats merchantShortId opCity apiTokenInfo from to search limit offset sortDesc sortOn responseType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId Nothing
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetDriverListStats) fleetOwnerId from to search limit offset sortDesc sortOn responseType

---------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------- READ LAYER (Multi Fleet Level) --------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------

getDriverFleetGetAllBadge :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe DFBT.FleetBadgeType -> Maybe Bool -> Maybe Text -> Flow Common.FleetBadgeResT
getDriverFleetGetAllBadge merchantShortId opCity apiTokenInfo mblimit mboffset mbSearchString mbFleetOwnerId mbBadgeType mbIsActive _ = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let memberPersonId = apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetGetAllBadge) mblimit mboffset mbSearchString mbFleetOwnerId mbBadgeType mbIsActive (Just memberPersonId)

getDriverFleetGetAllVehicle :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Text -> Flow Common.ListVehicleResT
getDriverFleetGetAllVehicle merchantShortId opCity apiTokenInfo mbLimit mbOffset mbRegNumberString mbFleetOwnerId mbIsActive _ = do
  checkFleetOwnerVerification apiTokenInfo.personId
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let memberPersonId = apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetGetAllVehicle) mbLimit mbOffset mbRegNumberString mbFleetOwnerId mbIsActive (Just memberPersonId)

getDriverFleetGetAllDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Text -> Flow Common.FleetListDriverResT
getDriverFleetGetAllDriver merchantShortId opCity apiTokenInfo mbLimit mbOffset mbMobileNumber mbName mbSearchString mbFleetOwnerId mbIsActive _ = do
  checkFleetOwnerVerification apiTokenInfo.personId
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let memberPersonId = apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetGetAllDriver) mbLimit mbOffset mbMobileNumber mbName mbSearchString mbFleetOwnerId mbIsActive (Just memberPersonId)

getDriverFleetTripTransactions :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Maybe Text -> Int -> Int -> Flow Common.TripTransactionRespT
getDriverFleetTripTransactions merchantShortId opCity apiTokenInfo driverId mbFrom mbTo mbVehicleNumber mbFleetOwnerId _ limit offset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let memberPersonId = apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetTripTransactions) driverId mbFrom mbTo mbVehicleNumber mbFleetOwnerId (Just memberPersonId) limit offset

getDriverFleetDriverAssociation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Maybe Common.DriverMode -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Bool -> Flow Common.DrivertoVehicleAssociationResT
getDriverFleetDriverAssociation merhcantId opCity apiTokenInfo mbIsActive mbLimit mbOffset mbCountryCode mbPhoneNo mbStats mbFrom mbTo mbMode name mbSearchString mbFleetOwnerId _ _ _ = do
  checkFleetOwnerVerification apiTokenInfo.personId
  checkedMerchantId <- merchantCityAccessCheck merhcantId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
      isRequestorFleerOwner = DP.isFleetOwner apiTokenInfo.person
      hasFleetMemberHierarchy = apiTokenInfo.merchant.hasFleetMemberHierarchy
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetDriverAssociation) mbIsActive mbLimit mbOffset mbCountryCode mbPhoneNo mbStats mbFrom mbTo mbMode name mbSearchString mbFleetOwnerId (Just requestorId) hasFleetMemberHierarchy (Just isRequestorFleerOwner)

getDriverFleetVehicleAssociation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Maybe Common.FleetVehicleStatus -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Bool -> Flow Common.DrivertoVehicleAssociationResT
getDriverFleetVehicleAssociation merhcantId opCity apiTokenInfo mbLimit mbOffset mbVehicleNo mbIncludeStats mbFrom mbTo mbStatus mbSearchString mbStatusAwareVehicleNo mbFleetOwnerId _ _ _ = do
  checkFleetOwnerVerification apiTokenInfo.personId
  checkedMerchantId <- merchantCityAccessCheck merhcantId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
      isRequestorFleerOwner = DP.isFleetOwner apiTokenInfo.person
      hasFleetMemberHierarchy = apiTokenInfo.merchant.hasFleetMemberHierarchy
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetVehicleAssociation) mbLimit mbOffset mbVehicleNo mbIncludeStats mbFrom mbTo mbStatus mbSearchString mbStatusAwareVehicleNo mbFleetOwnerId (Just requestorId) hasFleetMemberHierarchy (Just isRequestorFleerOwner)

getDriverFleetGetDriverRequests :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe UTCTime -> Maybe UTCTime -> Maybe AlertRequestType -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe AlertRequestStatus -> Maybe Int -> Maybe Int -> Flow Common.DriverRequestRespT
getDriverFleetGetDriverRequests merchantShortId opCity apiTokenInfo mbFrom mbTo mbAlertRequestType mbRouteCode mbDriverId mbBadgeName _ mbalertStatus mbLimit mbOffset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let memberPersonId = apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetGetDriverRequests) mbFrom mbTo mbAlertRequestType mbRouteCode mbDriverId mbBadgeName (Just memberPersonId) mbalertStatus mbLimit mbOffset

postDriverFleetGetNearbyDrivers :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.NearbyDriverReq -> Flow Common.NearbyDriverRespT
postDriverFleetGetNearbyDrivers merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerIds <- getFleetOwnerIds apiTokenInfo.personId.getId Nothing
  drivers <-
    concatMapM
      ( \(fleetOwnerId', fleetOwnerName) -> do
          Common.NearbyDriverResp {..} <- Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetGetNearbyDrivers) fleetOwnerId' req
          return $ map (addFleetOwnerDetails fleetOwnerId' fleetOwnerName) drivers
      )
      fleetOwnerIds
  return $ Common.NearbyDriverRespT {..}
  where
    addFleetOwnerDetails fleetOwnerId fleetOwnerName Common.DriverInfo {..} = Common.DriverInfoT {..}

getDriverDashboardInternalHelperGetFleetOwnerId :: (ShortId DM.Merchant -> City.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Text -> Environment.Flow Kernel.Prelude.Text)
getDriverDashboardInternalHelperGetFleetOwnerId _ _ _ _ = throwError $ InternalError "Unimplemented!"

getDriverDashboardInternalHelperGetFleetOwnerIds :: (ShortId DM.Merchant -> City.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Text -> Environment.Flow [(Kernel.Prelude.Text, Kernel.Prelude.Text)])
getDriverDashboardInternalHelperGetFleetOwnerIds _ _ _ _ = throwError $ InternalError "Unimplemented!"

postDriverFleetV2AccessMultiOwnerIdSelect :: (ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Bool -> Common.MultiOwnerSelect -> Environment.Flow APISuccess)
postDriverFleetV2AccessMultiOwnerIdSelect merchantShortId opCity apiTokenInfo _ onlyCurrent enable req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let fleetMemberId = apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetV2AccessMultiOwnerIdSelect) (Just fleetMemberId) onlyCurrent enable req

getDriverFleetBookings :: (Kernel.Types.Id.ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.Flow Common.FleetBookingsInformationResponse)
getDriverFleetBookings merchantShortId opCity apiTokenInfo limit offset from to status vehicleNo searchByFleetOwnerId mbSearchByTicketPlaceId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let memberPersonId = apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetBookings) memberPersonId limit offset from to status vehicleNo searchByFleetOwnerId mbSearchByTicketPlaceId

getDriverFleetAssignments :: (Kernel.Types.Id.ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow Common.FleetBookingAssignmentsResponse)
getDriverFleetAssignments merchantShortId opCity apiTokenInfo limit offset from to vehicleNo mainAssignmentId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let memberPersonId = apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetAssignments) memberPersonId limit offset from to vehicleNo mainAssignmentId

getDriverFleetOperatorInfo :: (Kernel.Types.Id.ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow Common.FleetOwnerInfoRes)
getDriverFleetOperatorInfo merchantShortId opCity apiTokenInfo mbMobileCountryCode mbMobileNumber mbPersonId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city

  person <- case (mbPersonId, mbMobileNumber) of
    (Just pid, _) -> QP.findById (Id pid) >>= fromMaybeM (PersonNotFound pid)
    (_, Just mobileNumber) -> do
      let mobileCountryCode = fromMaybe DCommon.mobileIndianCode mbMobileCountryCode
      QP.findByMobileNumber mobileNumber mobileCountryCode >>= fromMaybeM (PersonNotFound mobileNumber)
    (Nothing, Nothing) ->
      throwError $ InvalidRequest "Either personId or mobile number must be provided."
  res <- Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetOperatorInfo) person.id.getId

  pure
    res {Common.approvedBy = person.approvedBy <&> getId}

postDriverFleetLocationList :: (Kernel.Types.Id.ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.DriverLocationListReq -> Environment.Flow Common.DriverLocationListResp)
postDriverFleetLocationList merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let memberPersonId = apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetLocationList) memberPersonId req

postDriverFleetGetDriverDetails :: (Kernel.Types.Id.ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.DriverDetailsReq -> Environment.Flow Common.DriverDetailsResp)
postDriverFleetGetDriverDetails merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let memberPersonId = apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetGetDriverDetails) memberPersonId req

postDriverFleetGetNearbyDriversV2 :: (Kernel.Types.Id.ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.NearbyDriversReqV2 -> Environment.Flow Common.NearbyDriversRespTV2)
postDriverFleetGetNearbyDriversV2 merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerIds <- getFleetOwnerIds apiTokenInfo.personId.getId Nothing
  drivers <-
    concatMapM
      ( \(fleetOwnerId', fleetOwnerName) -> do
          Common.NearbyDriversRespV2 {..} <- Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetGetNearbyDriversV2) fleetOwnerId' req
          return $ map (addFleetOwnerDetails fleetOwnerId' fleetOwnerName) drivers
      )
      fleetOwnerIds
  return $ Common.NearbyDriversRespTV2 {..}
  where
    addFleetOwnerDetails fleetOwnerId fleetOwnerName Common.NearbyDriverDetails {..} = Common.NearbyDriverDetailsT {..}

getDriverDashboardFleetTripWaypoints :: (Kernel.Types.Id.ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Kernel.Types.Id.Id Common.TripTransaction -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow Common.TripTransactionWaypointsRes)
getDriverDashboardFleetTripWaypoints merchantShortId opCity apiTokenInfo tripTransactionId fleetOwnerId mbliteMode _ = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId' <- getFleetOwnerId apiTokenInfo.personId.getId (Just fleetOwnerId)
  let memberPersonId = apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverDashboardFleetTripWaypoints) tripTransactionId fleetOwnerId' mbliteMode (Just memberPersonId)

postDriverDashboardFleetEstimateRoute :: (Kernel.Types.Id.ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Common.EstimateRouteReq -> Environment.Flow Kernel.External.Maps.GetRoutesResp)
postDriverDashboardFleetEstimateRoute merchantShortId opCity apiTokenInfo _ req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId' <- getFleetOwnerId apiTokenInfo.personId.getId Nothing
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverDashboardFleetEstimateRoute) fleetOwnerId' req

postDriverFleetTripTransactionsV2 :: (Kernel.Types.Id.ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe Common.TripStatus -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Environment.Flow Common.TripTransactionRespT)
postDriverFleetTripTransactionsV2 merchantShortId opCity apiTokenInfo mbFrom mbTo mbVehicleNumber fleetOwnerId _ mbStatus mbDriverId mbDutyType limit offset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let memberPersonId = apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetTripTransactionsV2) mbFrom mbTo mbVehicleNumber fleetOwnerId (Just memberPersonId) mbStatus mbDriverId mbDutyType limit offset

postDriverFleetApproveDriver :: (Kernel.Types.Id.ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Common.ApproveDriverReq -> Environment.Flow APISuccess)
postDriverFleetApproveDriver merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just req.driverId) (Just req)
  T.withTransactionStoring transaction $ do
    fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId Nothing
    Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetApproveDriver) fleetOwnerId req

postDriverFleetDriverUpdate :: (Kernel.Types.Id.ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Kernel.Types.Id.Id Common.Driver -> Common.UpdateDriverReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postDriverFleetDriverUpdate merchantShortId opCity apiTokenInfo driverId req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo (Just driverId) (Just req)
  T.withTransactionStoring transaction $ do
    fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId Nothing
    Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetDriverUpdate) driverId fleetOwnerId req

getDriverFleetVehicleListStats :: (Kernel.Types.Id.ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Data.Time.Day -> Data.Time.Day -> Environment.Flow Common.FleetVehicleStatsRes)
getDriverFleetVehicleListStats merchantShortId opCity apiTokenInfo vehicleNo limit offset from to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let fleetOwnerId = apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetVehicleListStats) fleetOwnerId vehicleNo limit offset from to

getDriverFleetDriverOnboardedDriversAndUnlinkedVehicles :: (Kernel.Types.Id.ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.Flow Common.OnboardedDriversAndUnlinkedVehiclesRes)
getDriverFleetDriverOnboardedDriversAndUnlinkedVehicles merchantShortId opCity apiTokenInfo fleetOwnerId limit offset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId' <- getFleetOwnerId apiTokenInfo.personId.getId (Just fleetOwnerId)
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetDriverOnboardedDriversAndUnlinkedVehicles) fleetOwnerId' limit offset

getDriverFleetDriverDetails :: (Kernel.Types.Id.ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Kernel.Types.Id.Id Common.Driver -> Environment.Flow Common.DriverDetailsRes)
getDriverFleetDriverDetails merchantShortId opCity apiTokenInfo driverId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let fleetOwnerId = apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetDriverDetails) fleetOwnerId driverId
