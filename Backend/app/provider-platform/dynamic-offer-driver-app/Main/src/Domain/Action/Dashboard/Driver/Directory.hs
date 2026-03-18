{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Driver.Directory
  ( listDirectoryDrivers,
    getDirectoryDriverProfile,
    updateDirectoryDriverStatus,
    bulkApproveDirectoryDrivers,
    getDirectoryStats,
    exportDirectoryDrivers,
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error

--------------------------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------------------------

data DirectoryAdminStatus = Pending | Approved | Suspended | Flagged
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)

data DirectoryDriverListItem = DirectoryDriverListItem
  { driverId :: Id DP.Person,
    firstName :: Text,
    lastName :: Maybe Text,
    mobileNumber :: Maybe Text,
    cityName :: Maybe Text,
    fleetOwnerName :: Maybe Text,
    fleetOwnerId :: Maybe (Id DP.Person),
    vehicleNumber :: Maybe Text,
    vehicleVariant :: Maybe Text,
    rating :: Maybe Int,
    totalRidesCompleted :: Int,
    onboardingStatus :: Maybe Text,
    adminStatus :: Text,
    profileCompleteness :: Int,
    trainingModulesCompleted :: Int,
    trainingTotalModules :: Int,
    enrolledAt :: Maybe UTCTime,
    joinedAt :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data DirectoryDriverListResponse = DirectoryDriverListResponse
  { drivers :: [DirectoryDriverListItem],
    totalCount :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data DirectoryDriverProfile = DirectoryDriverProfile
  { driverId :: Id DP.Person,
    personalInfo :: PersonalInfo,
    directoryInfo :: DirectoryInfo,
    fleetInfo :: Maybe FleetInfo,
    vehicleInfo :: Maybe VehicleInfo,
    trainingProgress :: TrainingProgress,
    performance :: PerformanceInfo,
    documents :: DocumentsInfo,
    auditLog :: [AuditLogEntry]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data PersonalInfo = PersonalInfo
  { firstName :: Text,
    lastName :: Maybe Text,
    mobileNumber :: Maybe Text,
    email :: Maybe Text,
    joinedAt :: UTCTime,
    enabled :: Bool,
    blocked :: Bool
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data DirectoryInfo = DirectoryInfo
  { adminStatus :: Text,
    enrolledAt :: Maybe UTCTime,
    profileCompleteness :: Int,
    adminReviewedAt :: Maybe UTCTime,
    adminReviewedBy :: Maybe Text,
    adminNotes :: Maybe Text,
    skills :: [Text],
    availability :: Maybe Value,
    bio :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data FleetInfo = FleetInfo
  { fleetOwnerId :: Id DP.Person,
    fleetOwnerName :: Text,
    assignedSince :: Maybe UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data VehicleInfo = VehicleInfo
  { vehicleNumber :: Text,
    variant :: Maybe Text,
    model :: Maybe Text,
    color :: Maybe Text,
    registrationDate :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data TrainingProgress = TrainingProgress
  { modulesCompleted :: Int,
    totalModules :: Int,
    modules :: [TrainingModuleInfo]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data TrainingModuleInfo = TrainingModuleInfo
  { moduleId :: Text,
    moduleName :: Text,
    status :: Text,
    completedAt :: Maybe UTCTime,
    score :: Maybe Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data PerformanceInfo = PerformanceInfo
  { rating :: Maybe Int,
    totalRides :: Int,
    completionRate :: Maybe Double,
    averageRideRating :: Maybe Double,
    cancellationRate :: Maybe Double
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data DocumentsInfo = DocumentsInfo
  { drivingLicense :: Maybe DocumentStatus,
    vehicleRC :: Maybe DocumentStatus,
    insurance :: Maybe DocumentStatus,
    permit :: Maybe DocumentStatus
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data DocumentStatus = DocumentStatus
  { status :: Text,
    expiresAt :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data AuditLogEntry = AuditLogEntry
  { action :: Text,
    performedBy :: Text,
    reason :: Maybe Text,
    previousStatus :: Maybe Text,
    newStatus :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data UpdateStatusRequest = UpdateStatusRequest
  { status :: Text,
    notes :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data UpdateStatusResponse = UpdateStatusResponse
  { driverId :: Id DP.Person,
    previousStatus :: Text,
    newStatus :: Text,
    updatedAt :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data BulkApproveRequest = BulkApproveRequest
  { driverIds :: [Id DP.Person],
    notes :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data BulkApproveResponse = BulkApproveResponse
  { approved :: Int,
    failed :: Int,
    errors :: [Text]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data DirectoryStatsResponse = DirectoryStatsResponse
  { totalEnrolled :: Int,
    pending :: Int,
    approved :: Int,
    suspended :: Int,
    flagged :: Int,
    avgProfileCompleteness :: Double
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data DirectoryListFilters = DirectoryListFilters
  { cityId :: Maybe Text,
    fleetOwnerId :: Maybe Text,
    vehicleVariant :: Maybe Text,
    onboardingStatus :: Maybe Text,
    adminStatus :: Maybe Text,
    trainingComplete :: Maybe Bool,
    ratingMin :: Maybe Int,
    ratingMax :: Maybe Int,
    search :: Maybe Text,
    sortBy :: Maybe Text,
    sortOrder :: Maybe Text,
    limit :: Maybe Int,
    offset :: Maybe Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

--------------------------------------------------------------------------------------------------
-- API Implementations
--------------------------------------------------------------------------------------------------

listDirectoryDrivers ::
  ShortId DM.Merchant ->
  Context.City ->
  DirectoryListFilters ->
  Flow DirectoryDriverListResponse
listDirectoryDrivers merchantShortId opCity _filters = do
  _merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityDoesNotExist $ "merchantShortId: " <> merchantShortId.getShortId <> ", opCity: " <> show opCity)
  -- TODO: Implement actual DB query against driver_directory_view with filters
  logInfo "Driver Directory: listDirectoryDrivers called"
  pure $ DirectoryDriverListResponse {drivers = [], totalCount = 0}

getDirectoryDriverProfile ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DP.Person ->
  Flow DirectoryDriverProfile
getDirectoryDriverProfile merchantShortId opCity driverId = do
  _merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityDoesNotExist $ "merchantShortId: " <> merchantShortId.getShortId <> ", opCity: " <> show opCity)
  -- TODO: Implement actual DB queries to build full profile
  logInfo $ "Driver Directory: getDirectoryDriverProfile called for " <> driverId.getId
  now <- getCurrentTime
  pure $
    DirectoryDriverProfile
      { driverId = driverId,
        personalInfo =
          PersonalInfo
            { firstName = "",
              lastName = Nothing,
              mobileNumber = Nothing,
              email = Nothing,
              joinedAt = now,
              enabled = True,
              blocked = False
            },
        directoryInfo =
          DirectoryInfo
            { adminStatus = "pending",
              enrolledAt = Nothing,
              profileCompleteness = 0,
              adminReviewedAt = Nothing,
              adminReviewedBy = Nothing,
              adminNotes = Nothing,
              skills = [],
              availability = Nothing,
              bio = Nothing
            },
        fleetInfo = Nothing,
        vehicleInfo = Nothing,
        trainingProgress = TrainingProgress {modulesCompleted = 0, totalModules = 0, modules = []},
        performance = PerformanceInfo {rating = Nothing, totalRides = 0, completionRate = Nothing, averageRideRating = Nothing, cancellationRate = Nothing},
        documents = DocumentsInfo {drivingLicense = Nothing, vehicleRC = Nothing, insurance = Nothing, permit = Nothing},
        auditLog = []
      }

updateDirectoryDriverStatus ::
  ShortId DM.Merchant ->
  Context.City ->
  Id DP.Person ->
  UpdateStatusRequest ->
  Flow UpdateStatusResponse
updateDirectoryDriverStatus merchantShortId opCity driverId req = do
  _merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityDoesNotExist $ "merchantShortId: " <> merchantShortId.getShortId <> ", opCity: " <> show opCity)
  -- TODO: Implement actual status update with audit logging
  logInfo $ "Driver Directory: updateDirectoryDriverStatus to " <> req.status <> " for " <> driverId.getId
  now <- getCurrentTime
  pure $
    UpdateStatusResponse
      { driverId = driverId,
        previousStatus = "pending",
        newStatus = req.status,
        updatedAt = now
      }

bulkApproveDirectoryDrivers ::
  ShortId DM.Merchant ->
  Context.City ->
  BulkApproveRequest ->
  Flow BulkApproveResponse
bulkApproveDirectoryDrivers merchantShortId opCity req = do
  _merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityDoesNotExist $ "merchantShortId: " <> merchantShortId.getShortId <> ", opCity: " <> show opCity)
  -- TODO: Implement bulk approval with audit logging
  logInfo $ "Driver Directory: bulkApproveDirectoryDrivers called for " <> show (length req.driverIds) <> " drivers"
  pure $
    BulkApproveResponse
      { approved = length req.driverIds,
        failed = 0,
        errors = []
      }

getDirectoryStats ::
  ShortId DM.Merchant ->
  Context.City ->
  Flow DirectoryStatsResponse
getDirectoryStats merchantShortId opCity = do
  _merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityDoesNotExist $ "merchantShortId: " <> merchantShortId.getShortId <> ", opCity: " <> show opCity)
  -- TODO: Implement actual stats aggregation query
  logInfo "Driver Directory: getDirectoryStats called"
  pure $
    DirectoryStatsResponse
      { totalEnrolled = 0,
        pending = 0,
        approved = 0,
        suspended = 0,
        flagged = 0,
        avgProfileCompleteness = 0.0
      }

exportDirectoryDrivers ::
  ShortId DM.Merchant ->
  Context.City ->
  DirectoryListFilters ->
  Flow Text
exportDirectoryDrivers merchantShortId opCity _filters = do
  _merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityDoesNotExist $ "merchantShortId: " <> merchantShortId.getShortId <> ", opCity: " <> show opCity)
  -- TODO: Implement CSV export generation
  logInfo "Driver Directory: exportDirectoryDrivers called"
  pure "driverId,firstName,lastName,mobileNumber,adminStatus,profileCompleteness\n"
