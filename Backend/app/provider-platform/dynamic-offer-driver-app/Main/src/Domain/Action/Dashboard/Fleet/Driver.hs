{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.Dashboard.Fleet.Driver
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
    postDriverFleetAddDrivers,
    postDriverFleetConsent,
    getDriverFleetRoutes,
    getDriverFleetPossibleRoutes,
    postDriverFleetTripPlanner,
    getDriverFleetTripTransactions,
    postDriverFleetAddDriverBusRouteMapping,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as Common
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverRegistration as Common
import Data.Time
import qualified Domain.Action.Dashboard.Driver as DDriver
import qualified Domain.Action.Dashboard.Fleet.Registration as Fleet
import qualified Domain.Action.UI.Ride as DARide
import qualified Domain.Types.Merchant as DM
import Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Servant hiding (throwError)
import Storage.Beam.SystemConfigs ()

-- TODO Domain.Action.Dashboard.Fleet.Operations

postDriverFleetAddVehicle ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Text ->
  Maybe Text ->
  Common.AddVehicleReq ->
  Flow APISuccess
postDriverFleetAddVehicle merchantShortId opCity phoneNo fleetOwnerId mbMobileCountryCode =
  DDriver.addVehicleForFleet merchantShortId opCity phoneNo mbMobileCountryCode fleetOwnerId

postDriverFleetAddRCWithoutDriver ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.RegisterRCReq ->
  Flow APISuccess
postDriverFleetAddRCWithoutDriver = DDriver.registerRCForFleetWithoutDriver

postDriverFleetAddDrivers ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.CreateDriversReq ->
  Flow APISuccess
postDriverFleetAddDrivers = DDriver.addDriversInFleet

getDriverFleetGetAllVehicle ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Int ->
  Maybe Int ->
  Flow Common.ListVehicleRes
getDriverFleetGetAllVehicle = DDriver.getAllVehicleForFleet

getDriverFleetGetAllDriver ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Int ->
  Maybe Int ->
  Flow Common.FleetListDriverRes
getDriverFleetGetAllDriver merchantShortId _opCity = DDriver.getAllDriverForFleet merchantShortId

postDriverFleetUnlink ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Id Common.Driver ->
  Text ->
  Flow APISuccess
postDriverFleetUnlink merchantShortId _opCity = DDriver.fleetUnlinkVehicle merchantShortId

postDriverFleetRemoveVehicle ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Text ->
  Flow APISuccess
postDriverFleetRemoveVehicle = DDriver.fleetRemoveVehicle

postDriverFleetRemoveDriver ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Id Common.Driver ->
  Flow APISuccess
postDriverFleetRemoveDriver = DDriver.fleetRemoveDriver

getDriverFleetTotalEarning ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Flow Common.FleetTotalEarningResponse
getDriverFleetTotalEarning = DDriver.fleetTotalEarning

getDriverFleetVehicleEarning ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Flow Common.FleetEarningListRes
getDriverFleetVehicleEarning = DDriver.fleetVehicleEarning

getDriverFleetDriverEarning ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Bool ->
  Maybe Common.SortOn ->
  Flow Common.FleetEarningListRes
getDriverFleetDriverEarning = DDriver.fleetDriverEarning

getDriverFleetDriverVehicleAssociation ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Flow Common.DrivertoVehicleAssociationRes
getDriverFleetDriverVehicleAssociation = DDriver.getFleetDriverVehicleAssociation

getDriverFleetDriverAssociation ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Bool ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Common.DriverMode ->
  Flow Common.DrivertoVehicleAssociationRes
getDriverFleetDriverAssociation merchantShortId opCity fleetOwnerId mbIsActive mbLimit mbOffset mbCountryCode mbPhoneNo =
  DDriver.getFleetDriverAssociation merchantShortId opCity fleetOwnerId mbLimit mbOffset mbCountryCode mbPhoneNo mbIsActive

getDriverFleetVehicleAssociation ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe Text ->
  Maybe Bool ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Common.FleetVehicleStatus ->
  Flow Common.DrivertoVehicleAssociationRes
getDriverFleetVehicleAssociation = DDriver.getFleetVehicleAssociation

postDriverFleetVehicleDriverRcStatus ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Text ->
  Common.RCStatusReq ->
  Flow APISuccess
postDriverFleetVehicleDriverRcStatus = DDriver.setVehicleDriverRcStatusForFleet

postDriverUpdateFleetOwnerInfo ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Common.UpdateFleetOwnerInfoReq ->
  Flow APISuccess
postDriverUpdateFleetOwnerInfo = DDriver.updateFleetOwnerInfo

getDriverFleetOwnerInfo ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.Driver ->
  Flow Common.FleetOwnerInfoRes
getDriverFleetOwnerInfo = DDriver.getFleetOwnerInfo

postDriverFleetSendJoiningOtp ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.AuthReq ->
  Flow Common.AuthRes
postDriverFleetSendJoiningOtp = Fleet.sendFleetJoiningOtp

postDriverFleetVerifyJoiningOtp ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe Text ->
  Common.VerifyFleetJoiningOtpReq ->
  Flow APISuccess
postDriverFleetVerifyJoiningOtp = Fleet.verifyFleetJoiningOtp

postDriverFleetLinkRCWithDriver ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.LinkRCWithDriverForFleetReq ->
  Flow APISuccess
postDriverFleetLinkRCWithDriver = DDriver.linkRCWithDriverForFleet

postDriverFleetConsent ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Text ->
  Text ->
  Flow APISuccess
postDriverFleetConsent = DDriver.postDriverFleetConsent

getDriverFleetRoutes ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe LatLong ->
  Maybe Text ->
  Flow Common.RouteAPIResp
getDriverFleetRoutes = DDriver.getDriverFleetRoutes

getDriverFleetPossibleRoutes ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  LatLong ->
  Flow Common.RouteAPIResp
getDriverFleetPossibleRoutes = DDriver.getDriverFleetPossibleRoutes

postDriverFleetTripPlanner ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Id Common.Driver ->
  Text ->
  Common.TripPlannerReq ->
  Flow APISuccess
postDriverFleetTripPlanner = DDriver.postDriverFleetTripPlanner

getDriverFleetTripTransactions ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Id Common.Driver ->
  Text ->
  Maybe Day ->
  Flow Common.TripTransactionResp
getDriverFleetTripTransactions = DDriver.getDriverFleetTripTransactions

postDriverFleetAddDriverBusRouteMapping ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.CreateDriverBusRouteMappingReq ->
  Flow APISuccess
postDriverFleetAddDriverBusRouteMapping = DDriver.addDriverBusRouteMapping
