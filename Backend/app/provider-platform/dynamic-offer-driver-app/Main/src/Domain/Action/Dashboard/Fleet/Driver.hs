{-# OPTIONS_GHC -Wno-orphans #-}

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
    getDriverFleetGetDriverRequests,
    postDriverFleetRespondDriverRequest,
    postDriverFleetAddVehicles,
    putDriverDashboardFleetWmbTripDelete,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as Common
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverRegistration as Common
import Data.Time
import qualified Domain.Action.Dashboard.Driver as DDriver
import qualified Domain.Action.Dashboard.Fleet.Registration as Fleet
import qualified Domain.Types.Merchant as DM
import Domain.Types.TripTransaction
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Storage.Beam.SystemConfigs ()
import qualified Storage.Queries.TripTransaction as QTT

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

getDriverFleetGetDriverRequests ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe Common.RequestStatus ->
  Maybe Int ->
  Maybe Int ->
  Flow Common.DriverRequestResp
getDriverFleetGetDriverRequests = DDriver.getDriverRequests

postDriverFleetRespondDriverRequest ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.RequestRespondReq ->
  Flow APISuccess
postDriverFleetRespondDriverRequest = DDriver.respondDriverRequest

postDriverFleetAddRCWithoutDriver ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.RegisterRCReq ->
  Flow APISuccess
postDriverFleetAddRCWithoutDriver = DDriver.registerRCForFleetWithoutDriver

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

postDriverFleetAddVehicles ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.CreateVehiclesReq ->
  Flow APISuccess
postDriverFleetAddVehicles = DDriver.addVehiclesInFleet

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

putDriverDashboardFleetWmbTripDelete ::
  ShortId DM.Merchant ->
  Context.City ->
  Id Common.TripTransaction ->
  Text ->
  Flow APISuccess
putDriverDashboardFleetWmbTripDelete _ _ tripTransactionId _ = do
  QTT.updateStatus CANCELLED Nothing (cast @Common.TripTransaction @TripTransaction tripTransactionId)
  -- TODO : add notification to driver
  pure Success
