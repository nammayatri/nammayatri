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
    postDriverFleetAddVehicle,
    postDriverFleetAddRCWithoutDriver,
    getDriverFleetGetAllVehicle,
    getDriverFleetGetAllDriver,
    postDriverFleetUnlink,
    postDriverFleetRemoveVehicle,
    getFleetOwnerId,
    getFleetOwnerIds,
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
import qualified Data.ByteString.Lazy as LBS
import Data.Text hiding (elem, filter, find, length, map, null)
import "lib-dashboard" Domain.Action.Dashboard.Person as DPerson
import Domain.Action.ProviderPlatform.CheckVerification (checkFleetOwnerVerification)
import Domain.Types.Alert
import Domain.Types.FleetMemberAssociation as DFMA
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import qualified Domain.Types.Role as DRole
import qualified "lib-dashboard" Domain.Types.Transaction as DT
import "lib-dashboard" Environment
import EulerHS.Prelude hiding (elem, find, length, map, null, whenJust)
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
import qualified Storage.Queries.FleetMemberAssociation as FMA
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
getFleetOwnerId memberPersonId mbFleetOwnerId = do
  maybe
    ( FMA.findAllActiveByfleetMemberId memberPersonId True
        >>= \case
          [] -> return memberPersonId
          [DFMA.FleetMemberAssociation {..}] -> return fleetOwnerId
          _ -> throwError AccessDenied
    )
    identity
    ((verifyFleetOwnerAccess memberPersonId) <$> mbFleetOwnerId)

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
      if apiTokenInfo.person.dashboardAccessType `elem` [Just DRole.FLEET_OWNER, Just DRole.RENTAL_FLEET_OWNER]
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

data FleetOwnerInfo = FleetOwnerInfo
  { fleetOwnerId :: Text,
    fleetOwnerName :: Text,
    requestorId :: Text
  }

getFleetOwnersInfoMerchantBased :: ApiTokenInfo -> Maybe Text -> Flow [FleetOwnerInfo]
getFleetOwnersInfoMerchantBased apiTokenInfo mbFleetOwnerId = do
  case apiTokenInfo.merchant.hasFleetMemberHierarchy of
    Just False -> do
      -- MSIL: requestor is fleet owner or operator, access check on bpp side required!
      let requestor = apiTokenInfo.person
          requestorId = requestor.id.getId
      if apiTokenInfo.person.dashboardAccessType `elem` [Just DRole.FLEET_OWNER, Just DRole.RENTAL_FLEET_OWNER]
        then do
          -- requestor is fleet owner
          whenJust mbFleetOwnerId $ \fleetOwnerId ->
            unless (fleetOwnerId == requestorId) $ throwError AccessDenied
          let fleetOwnerName = requestor.firstName <> " " <> requestor.lastName
          return [FleetOwnerInfo {fleetOwnerId = requestorId, fleetOwnerName, requestorId}]
        else do
          -- requestor is operator
          fleetOwnerId <- mbFleetOwnerId & fromMaybeM (InvalidRequest "fleetOwnerId required")
          fleetOwner <- QP.findById (Id fleetOwnerId) >>= fromMaybeM (PersonNotFound fleetOwnerId)
          let fleetOwnerName = fleetOwner.firstName <> " " <> fleetOwner.lastName
          return [FleetOwnerInfo {fleetOwnerId, fleetOwnerName, requestorId}]
    _ -> do
      -- Existing flow: consider requestor the same as fleet owner, fleet member operates on befalf of fleet owner
      fleetOwnerIds <- getFleetOwnerIds apiTokenInfo.personId.getId mbFleetOwnerId
      return $ (\(fleetOwnerId, fleetOwnerName) -> FleetOwnerInfo {fleetOwnerId, fleetOwnerName, requestorId = fleetOwnerId}) <$> fleetOwnerIds

------------------------------------- Multiple Fleet Owners Access --------------------------------------
getFleetOwnerIds :: Text -> Maybe Text -> Flow [(Text, Text)]
getFleetOwnerIds memberPersonId mbFleetOwnerId = do
  maybe
    ( FMA.findAllActiveByfleetMemberId memberPersonId True
        >>= \case
          [] -> do
            person <- QP.findById (Id memberPersonId) >>= fromMaybeM (PersonNotFound memberPersonId)
            return [(memberPersonId, person.firstName <> " " <> person.lastName)]
          fleetMemberAssociations -> do
            mapM
              ( \DFMA.FleetMemberAssociation {..} -> do
                  fleetMemberAssociation <- FMA.findOneByFleetOwnerId fleetOwnerId True >>= fromMaybeM (PersonNotFound fleetOwnerId)
                  person <- QP.findById (Id fleetMemberAssociation.fleetMemberId) >>= fromMaybeM (PersonNotFound fleetMemberAssociation.fleetMemberId)
                  return (fleetOwnerId, person.firstName <> " " <> person.lastName)
              )
              fleetMemberAssociations
    )
    identity
    ( ( \fleetOwnerId -> do
          fleetMemberAssociation <- FMA.findOneByFleetOwnerId fleetOwnerId True >>= fromMaybeM (PersonNotFound fleetOwnerId)
          person <- QP.findById (Id fleetMemberAssociation.fleetMemberId) >>= fromMaybeM (PersonNotFound fleetMemberAssociation.fleetMemberId)
          return [(fleetOwnerId, person.firstName <> " " <> person.lastName)]
      )
        <$> mbFleetOwnerId
    )

------------------------------------- Verify Fleet Owners Access --------------------------------------
verifyFleetOwnerAccess :: Text -> Text -> Flow Text
verifyFleetOwnerAccess fleetMemberId accessedFleetOwnerId = do
  fleetOwnerIds <- getFleetOwnerIds fleetMemberId Nothing
  (fleetOwnerId, _) <- find (\(fleetOwnerId, _) -> fleetOwnerId == accessedFleetOwnerId) fleetOwnerIds & fromMaybeM AccessDenied
  -- let otherFleetOwnerIds = filter (\fleetOwnerId' -> fleetOwnerId' /= accessedFleetOwnerId) $ map fst fleetOwnerIds
  -- when (not $ null otherFleetOwnerIds) $
  --   FMA.updateFleetMembersActiveStatus False fleetMemberId otherFleetOwnerIds
  return fleetOwnerId

------------------------------------- Fleet Owners Access Control --------------------------------------
postDriverFleetAccessSelect :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> Maybe Bool -> Bool -> Flow APISuccess
postDriverFleetAccessSelect merchantShortId opCity apiTokenInfo fleetOwnerId mbOnlySingle enable = do
  _ <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let fleetMemberId = apiTokenInfo.personId.getId
      onlySingle = fromMaybe False mbOnlySingle
  when (onlySingle && enable) $ do
    fleetOwnerIds <- getFleetOwnerIds fleetMemberId Nothing
    FMA.updateFleetMembersActiveStatus False fleetMemberId (map fst fleetOwnerIds)
  FMA.updateFleetMemberActiveStatus enable fleetMemberId fleetOwnerId
  return Success

------------------------------------- Fleet Owners Access List --------------------------------------
getDriverFleetAccessList :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Flow Common.FleetOwnerListRes
getDriverFleetAccessList merchantShortId opCity apiTokenInfo = do
  _ <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let fleetMemberId = apiTokenInfo.personId.getId
  fleetOwners <- FMA.findAllByfleetMemberId fleetMemberId
  ownersList <-
    mapM
      ( \fleetMemberAssociation -> do
          fleetMemberAssociation' <- FMA.findOneByFleetOwnerId fleetMemberAssociation.fleetOwnerId True >>= fromMaybeM (PersonNotFound fleetMemberAssociation.fleetOwnerId)
          person <- QP.findById (Id fleetMemberAssociation'.fleetMemberId) >>= fromMaybeM (PersonNotFound fleetMemberAssociation'.fleetMemberId)
          return $
            Common.FleetOwnerListAPIEntity
              { fleetOwnerId = fleetMemberAssociation.fleetOwnerId,
                fleetOwnerName = person.firstName <> " " <> person.lastName,
                enabled = fleetMemberAssociation.enabled
              }
      )
      fleetOwners
  return $ Common.FleetOwnerListRes {..}

-------------------------------------------------------------------------------------------------------------
----------------------------------- WRITE LAYER (Single Fleet Level) ---------------------------------
-------------------------------------------------------------------------------------------------------------

postDriverFleetAddVehicle :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> Maybe Text -> Maybe Text -> Common.AddVehicleReq -> Flow APISuccess
postDriverFleetAddVehicle merchantShortId opCity apiTokenInfo phoneNo mbMobileCountryCode mbFleetOwnerId req = do
  checkFleetOwnerVerification apiTokenInfo.personId
  runRequestValidation Common.validateAddVehicleReq req
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  (fleetOwnerId, requestorId) <- getFleetOwnerAndRequestorIdMerchantBased apiTokenInfo mbFleetOwnerId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetAddVehicle) phoneNo requestorId (Just fleetOwnerId) mbMobileCountryCode req -- apiTokenInfo may contain opertaor or fleet

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
    fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId mbFleetOwnerId
    Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetVehicleDriverRcStatus) driverId fleetOwnerId req

postDriverUpdateFleetOwnerInfo :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Maybe Text -> Common.UpdateFleetOwnerInfoReq -> Flow APISuccess
postDriverUpdateFleetOwnerInfo merchantShortId opCity apiTokenInfo driverId mbFleetOwnerId req = do
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
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetLinkRCWithDriver) requestorId (Just fleetOwnerId) req

postDriverFleetAddVehicles :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Text -> Common.CreateVehiclesReq -> Flow Common.APISuccessWithUnprocessedEntities
postDriverFleetAddVehicles merchantShortId opCity apiTokenInfo mbFleetOwnerId req = do
  checkFleetOwnerVerification apiTokenInfo.personId
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- buildTransaction apiTokenInfo Nothing (Just req)
  (fleetOwnerId, requestorId) <- getFleetOwnerAndRequestorIdMerchantBased apiTokenInfo mbFleetOwnerId
  T.withTransactionStoring transaction $ Client.callFleetAPI checkedMerchantId opCity (Common.addMultipartBoundary "XXX00XXX" . (.driverDSL.postDriverFleetAddVehicles)) req{fleetOwnerId = Just fleetOwnerId, requestorId = Just requestorId}

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
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId mbFleetOwnerId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetUnlink) fleetOwnerId driverId vehicleNo

postDriverFleetRemoveVehicle :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Text -> Maybe Text -> Flow APISuccess
postDriverFleetRemoveVehicle merchantShortId opCity apiTokenInfo vehicleNo mbFleetOwnerId = do
  checkFleetOwnerVerification apiTokenInfo.personId
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId mbFleetOwnerId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetRemoveVehicle) fleetOwnerId vehicleNo

postDriverFleetRemoveDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Maybe Text -> Flow APISuccess
postDriverFleetRemoveDriver merchantShortId opCity apiTokenInfo driverId mbFleetOwnerId = do
  checkFleetOwnerVerification apiTokenInfo.personId
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId mbFleetOwnerId
  Client.callFleetAPI checkedMerchantId opCity (.driverDSL.postDriverFleetRemoveDriver) fleetOwnerId driverId

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

---------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------- READ LAYER (Multi Fleet Level) --------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------

getDriverFleetGetAllBadge :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Flow Common.FleetBadgeResT
getDriverFleetGetAllBadge merchantShortId opCity apiTokenInfo mblimit mboffset mbSearchString mbFleetOwnerId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerIds <- getFleetOwnerIds apiTokenInfo.personId.getId mbFleetOwnerId
  fleetBadgeInfos <-
    concatMapM
      ( \(fleetOwnerId', fleetOwnerName) -> do
          Common.FleetBadgeRes {..} <- Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetGetAllBadge) fleetOwnerId' mblimit mboffset mbSearchString
          return $ map (addFleetOwnerDetails fleetOwnerId' fleetOwnerName) fleetBadgeInfos
      )
      fleetOwnerIds
  return $ Common.FleetBadgeResT {..}
  where
    addFleetOwnerDetails fleetOwnerId fleetOwnerName Common.FleetBadgesAPIEntity {..} = Common.FleetBadgesAPIEntityT {..}

getDriverFleetGetAllVehicle :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Flow Common.ListVehicleResT
getDriverFleetGetAllVehicle merchantShortId opCity apiTokenInfo mbLimit mbOffset mbRegNumberString mbFleetOwnerId = do
  checkFleetOwnerVerification apiTokenInfo.personId
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerIds <- getFleetOwnerIds apiTokenInfo.personId.getId mbFleetOwnerId
  vehicles <-
    concatMapM
      ( \(fleetOwnerId', fleetOwnerName) -> do
          Common.ListVehicleRes {..} <- Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetGetAllVehicle) fleetOwnerId' mbLimit mbOffset mbRegNumberString
          return $ map (addFleetOwnerDetails fleetOwnerId' fleetOwnerName) vehicles
      )
      fleetOwnerIds
  return $ Common.ListVehicleResT {..}
  where
    addFleetOwnerDetails fleetOwnerId fleetOwnerName Common.VehicleAPIEntity {..} = Common.VehicleAPIEntityT {..}

getDriverFleetGetAllDriver :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Flow Common.FleetListDriverResT
getDriverFleetGetAllDriver merchantShortId opCity apiTokenInfo mbLimit mbOffset mbMobileNumber mbName mbSearchString mbFleetOwnerId = do
  checkFleetOwnerVerification apiTokenInfo.personId
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerIds <- getFleetOwnerIds apiTokenInfo.personId.getId mbFleetOwnerId
  fleetDriversInfos <-
    concatMapM
      ( \(fleetOwnerId', fleetOwnerName) -> do
          Common.FleetListDriverRes {..} <- Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetGetAllDriver) fleetOwnerId' mbLimit mbOffset mbMobileNumber mbName mbSearchString
          return $ map (addFleetOwnerDetails fleetOwnerId' fleetOwnerName) fleetDriversInfos
      )
      fleetOwnerIds
  return $ Common.FleetListDriverResT {..}
  where
    addFleetOwnerDetails fleetOwnerId fleetOwnerName Common.FleetDriversAPIEntity {..} = Common.FleetDriversAPIEntityT {..}

getDriverFleetTripTransactions :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Id Common.Driver -> Maybe UTCTime -> Maybe UTCTime -> Maybe Text -> Maybe Text -> Int -> Int -> Flow Common.TripTransactionRespT
getDriverFleetTripTransactions merchantShortId opCity apiTokenInfo driverId mbFrom mbTo mbVehicleNumber mbFleetOwnerId limit offset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerIds <- getFleetOwnerIds apiTokenInfo.personId.getId mbFleetOwnerId
  (trips, totalTrips) <-
    foldM
      ( \(tripsAcc, totalTripsAcc) (fleetOwnerId', fleetOwnerName) -> do
          Common.TripTransactionResp {..} <- Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetTripTransactions) fleetOwnerId' driverId mbFrom mbTo mbVehicleNumber limit offset
          return (tripsAcc <> map (addFleetOwnerDetails fleetOwnerId' fleetOwnerName) trips, totalTripsAcc + totalTrips)
      )
      ([], 0)
      fleetOwnerIds
  return $ Common.TripTransactionRespT {summary = Common.Summary {totalCount = 10000, count = totalTrips}, ..}
  where
    addFleetOwnerDetails fleetOwnerId fleetOwnerName Common.TripTransactionDetail {..} = Common.TripTransactionDetailT {..}

getDriverFleetDriverAssociation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Bool -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Maybe Common.DriverMode -> Maybe Text -> Maybe Text -> Maybe Text -> Flow Common.DrivertoVehicleAssociationResT
getDriverFleetDriverAssociation merhcantId opCity apiTokenInfo mbIsActive mbLimit mbOffset mbCountryCode mbPhoneNo mbStats mbFrom mbTo mbMode name mbSearchString mbFleetOwnerId = do
  checkFleetOwnerVerification apiTokenInfo.personId
  checkedMerchantId <- merchantCityAccessCheck merhcantId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnersInfo <- getFleetOwnersInfoMerchantBased apiTokenInfo mbFleetOwnerId
  listItem <-
    concatMapM
      ( \fleetOwnerInfo -> do
          Common.DrivertoVehicleAssociationRes {..} <- Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetDriverAssociation) fleetOwnerInfo.fleetOwnerId mbIsActive mbLimit mbOffset mbCountryCode mbPhoneNo mbStats mbFrom mbTo mbMode name mbSearchString (Just fleetOwnerInfo.requestorId)
          return $ map (addFleetOwnerDetails fleetOwnerInfo.fleetOwnerId fleetOwnerInfo.fleetOwnerName) listItem
      )
      fleetOwnersInfo
  return $
    Common.DrivertoVehicleAssociationResT
      { fleetOwnerId = apiTokenInfo.personId.getId, -- Kept for Backward Compatibility, Don't use this Fleet Owner Id for Multiple Fleets
        summary = Common.Summary {totalCount = 10000, count = length listItem},
        ..
      }
  where
    addFleetOwnerDetails fleetOwnerId fleetOwnerName Common.DriveVehicleAssociationListItem {..} = Common.DriveVehicleAssociationListItemT {..}

getDriverFleetVehicleAssociation :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Bool -> Maybe UTCTime -> Maybe UTCTime -> Maybe Common.FleetVehicleStatus -> Maybe Text -> Maybe Text -> Maybe Text -> Flow Common.DrivertoVehicleAssociationResT
getDriverFleetVehicleAssociation merhcantId opCity apiTokenInfo mbLimit mbOffset mbVehicleNo mbIncludeStats mbFrom mbTo mbStatus mbSearchString mbStatusAwareVehicleNo mbFleetOwnerId = do
  checkFleetOwnerVerification apiTokenInfo.personId
  checkedMerchantId <- merchantCityAccessCheck merhcantId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnersInfo <- getFleetOwnersInfoMerchantBased apiTokenInfo mbFleetOwnerId
  listItem <-
    concatMapM
      ( \fleetOwnerInfo -> do
          Common.DrivertoVehicleAssociationRes {..} <- Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetVehicleAssociation) fleetOwnerInfo.fleetOwnerId mbLimit mbOffset mbVehicleNo mbIncludeStats mbFrom mbTo mbStatus mbSearchString mbStatusAwareVehicleNo (Just fleetOwnerInfo.requestorId)
          return $ map (addFleetOwnerDetails fleetOwnerInfo.fleetOwnerId fleetOwnerInfo.fleetOwnerName) listItem
      )
      fleetOwnersInfo
  return $
    Common.DrivertoVehicleAssociationResT
      { fleetOwnerId = apiTokenInfo.personId.getId, -- Kept for Backward Compatibility, Don't use this Fleet Owner Id for Multiple Fleets
        summary = Common.Summary {totalCount = 10000, count = length listItem},
        ..
      }
  where
    addFleetOwnerDetails fleetOwnerId fleetOwnerName Common.DriveVehicleAssociationListItem {..} = Common.DriveVehicleAssociationListItemT {..}

getDriverFleetGetDriverRequests :: ShortId DM.Merchant -> City.City -> ApiTokenInfo -> Maybe UTCTime -> Maybe UTCTime -> Maybe AlertRequestType -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Flow Common.DriverRequestRespT
getDriverFleetGetDriverRequests merchantShortId opCity apiTokenInfo mbFrom mbTo mbAlertRequestType mbRouteCode mbDriverId mbBadgeName mbLimit mbOffset = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerIds <- getFleetOwnerIds apiTokenInfo.personId.getId Nothing
  requests <-
    concatMapM
      ( \(fleetOwnerId', fleetOwnerName) -> do
          Common.DriverRequestResp {..} <- Client.callFleetAPI checkedMerchantId opCity (.driverDSL.getDriverFleetGetDriverRequests) fleetOwnerId' mbFrom mbTo mbAlertRequestType mbRouteCode mbDriverId mbBadgeName mbLimit mbOffset
          return $ map (addFleetOwnerDetails fleetOwnerId' fleetOwnerName) requests
      )
      fleetOwnerIds
  return $ Common.DriverRequestRespT {..}
  where
    addFleetOwnerDetails fleetOwnerId fleetOwnerName Common.DriverRequestDetails {..} = Common.DriverRequestDetailsT {..}

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
