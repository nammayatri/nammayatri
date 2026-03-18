{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Fleet.DriverDirectory
  ( getFleetDriverDirectory,
    getFleetDriverDirectoryProfile,
    postFleetDriverDirectoryInvite,
    DriverDirectoryListResponse (..),
    DriverDirectoryItem (..),
    DriverDirectoryProfileResponse (..),
    DriverDirectoryInviteReq (..),
    DriverDirectoryInviteResp (..),
  )
where

import qualified Domain.Action.Dashboard.Fleet.Access as FleetAccess
import qualified Domain.Types.Common as DrInfo
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Person as DP
import qualified Domain.Types.VehicleCategory as DVC
import Environment
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.DriverDirectoryQueries as QDir
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.Person as QP
import Tools.Error

-- | Response types

data DriverDirectoryItem = DriverDirectoryItem
  { driverId :: Text,
    driverName :: Text,
    maskedPhone :: Text,
    vehicleCategory :: Maybe DVC.VehicleCategory,
    totalRidesCompleted :: Int,
    isOnline :: Bool,
    registeredAt :: UTCTime,
    hasExistingFleet :: Bool,
    cityName :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data DriverDirectoryListResponse = DriverDirectoryListResponse
  { drivers :: [DriverDirectoryItem],
    totalCount :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data DriverDirectoryProfileResponse = DriverDirectoryProfileResponse
  { driverId :: Text,
    driverName :: Text,
    maskedPhone :: Text,
    vehicleCategory :: Maybe DVC.VehicleCategory,
    totalRidesCompleted :: Int,
    isOnline :: Bool,
    memberSince :: UTCTime,
    hasExistingFleet :: Bool,
    pendingInviteFromThisFleet :: Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data DriverDirectoryInviteReq = DriverDirectoryInviteReq
  { reason :: Maybe Text,
    associationEndDate :: Maybe UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data DriverDirectoryInviteResp = DriverDirectoryInviteResp
  { associationId :: Text,
    status :: Text,
    message :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Get paginated driver directory for a fleet owner
getFleetDriverDirectory ::
  Maybe Text ->
  Text ->
  Text ->
  Maybe Text ->
  Maybe DVC.VehicleCategory ->
  Maybe Int ->
  Maybe Int ->
  Flow DriverDirectoryListResponse
getFleetDriverDirectory mbRequestorId fleetOwnerId cityId mbSearch mbVehicleCategory mbLimit mbOffset = do
  _fleetInfo <- FleetAccess.checkRequestorAccessToFleet False mbRequestorId fleetOwnerId
  let limit = min 50 (fromMaybe 10 mbLimit)
      offset = fromMaybe 0 mbOffset
  driversWithInfo <- QDir.findAllEligibleDriversForDirectory fleetOwnerId cityId mbSearch mbVehicleCategory limit offset
  totalCount <- QDir.countEligibleDriversForDirectory fleetOwnerId cityId mbSearch mbVehicleCategory
  let items = map (mkDirectoryItem cityId) driversWithInfo
  pure $ DriverDirectoryListResponse {drivers = items, totalCount}

-- | Get detailed profile of a single driver from the directory
getFleetDriverDirectoryProfile ::
  Maybe Text ->
  Text ->
  Text ->
  Text ->
  Flow DriverDirectoryProfileResponse
getFleetDriverDirectoryProfile mbRequestorId fleetOwnerId _cityId driverIdText = do
  _fleetInfo <- FleetAccess.checkRequestorAccessToFleet False mbRequestorId fleetOwnerId
  driver <- B.runInReplica $ QP.findById (Id driverIdText :: Id DP.Person) >>= fromMaybeM (PersonDoesNotExist driverIdText)
  -- Check for pending invite
  mbPendingAssoc <- QFDA.findByDriverIdAndFleetOwnerId (Id driverIdText) fleetOwnerId False
  let hasPending = isJust mbPendingAssoc
  pure $
    DriverDirectoryProfileResponse
      { driverId = driverIdText,
        driverName = driver.firstName <> maybe "" (" " <>) driver.lastName,
        maskedPhone = fromMaybe "XXXXXXXXXX" driver.maskedMobileDigits,
        vehicleCategory = Nothing,
        totalRidesCompleted = 0,
        isOnline = False,
        memberSince = driver.createdAt,
        hasExistingFleet = False,
        pendingInviteFromThisFleet = hasPending
      }

-- | Send an invite from fleet owner to a driver
postFleetDriverDirectoryInvite ::
  Maybe Text ->
  Text ->
  Text ->
  Text ->
  DriverDirectoryInviteReq ->
  Flow DriverDirectoryInviteResp
postFleetDriverDirectoryInvite mbRequestorId fleetOwnerId _cityId driverIdText req = do
  _fleetInfo <- FleetAccess.checkRequestorAccessToFleet False mbRequestorId fleetOwnerId
  -- Verify driver exists
  _driver <- B.runInReplica $ QP.findById (Id driverIdText :: Id DP.Person) >>= fromMaybeM (PersonDoesNotExist driverIdText)
  -- Check for existing active association
  mbActiveAssoc <- QFDA.findByDriverIdAndFleetOwnerId (Id driverIdText) fleetOwnerId True
  when (isJust mbActiveAssoc) $
    throwError (InvalidRequest "Driver is already linked to this fleet")
  -- Check for pending invite
  mbPendingAssoc <- QFDA.findByDriverIdAndFleetOwnerId (Id driverIdText) fleetOwnerId False
  when (isJust mbPendingAssoc) $
    throwError (InvalidRequest "An invite is already pending for this driver from this fleet")
  -- Create the association with isActive = False (pending invite)
  assocId <- generateGUID
  QFDA.createFleetDriverAssociationIfNotExists
    (Id driverIdText)
    (Id fleetOwnerId)
    Nothing
    DVC.AutoCategory
    False
    req.reason
  logInfo $ "Fleet directory invite sent: fleetOwner=" <> fleetOwnerId <> " driver=" <> driverIdText
  pure $
    DriverDirectoryInviteResp
      { associationId = assocId,
        status = "PENDING",
        message = "Invitation sent successfully. The driver will be notified."
      }

-- Helper functions

mkDirectoryItem :: Text -> (DP.Person, DI.DriverInformation) -> DriverDirectoryItem
mkDirectoryItem cityId (person, driverInfo) =
  DriverDirectoryItem
    { driverId = person.id.getId,
      driverName = person.firstName <> maybe "" (" " <>) person.lastName,
      maskedPhone = fromMaybe "XXXXXXXXXX" person.maskedMobileDigits,
      vehicleCategory = Nothing,
      totalRidesCompleted = 0,
      isOnline = driverInfo.mode == Just DrInfo.ONLINE,
      registeredAt = person.createdAt,
      hasExistingFleet = False,
      cityName = cityId
    }

