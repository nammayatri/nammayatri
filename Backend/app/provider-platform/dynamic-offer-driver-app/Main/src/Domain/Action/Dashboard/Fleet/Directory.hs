{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Fleet.Directory
  ( getFleetDirectorySearch,
    putDriverDirectoryVisibility,
  )
where

import qualified API.Types.UI.DriverDirectory as API
import qualified Domain.Types.DriverDirectoryProfile as DTDDP
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as SP
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common as KUC
import qualified Storage.Queries.DriverDirectoryProfile as QDDP
import qualified Storage.Queries.DriverStats as QDS
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Vehicle as QVeh

getFleetDirectorySearch ::
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe Text ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  Flow API.DirectorySearchRes
getFleetDirectorySearch _merchantId merchantOpCityId mbCity mbMinRating mbVehicleType mbAvailability mbSearchQuery mbLimit mbOffset = do
  let limit' = fromMaybe 20 mbLimit
      offset' = fromMaybe 0 mbOffset
  (profiles, totalCount) <- QDDP.searchListedDrivers merchantOpCityId mbCity mbMinRating mbVehicleType mbAvailability mbSearchQuery limit' offset'
  driverItems <- mapM buildDriverListItem profiles
  pure
    API.DirectorySearchRes
      { drivers = driverItems,
        totalCount = totalCount,
        limit = limit',
        offset = offset'
      }

putDriverDirectoryVisibility ::
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id SP.Person ->
  Bool ->
  Flow APISuccess
putDriverDirectoryVisibility _merchantId _merchantOpCityId driverId isVisible = do
  _person <- QP.findById driverId >>= fromMaybeM (PersonNotFound ("No person found with id " <> show driverId))
  now <- getCurrentTime
  existingProfile <- QDDP.findByDriverId driverId
  case existingProfile of
    Just _ -> QDDP.updateIsListedByDriverId isVisible (if isVisible then Just now else Nothing) now driverId
    Nothing -> logError ("No directory profile found for driver " <> show driverId <> ", cannot toggle visibility")
  pure Success

-- Internal helpers

buildDriverListItem :: DTDDP.DriverDirectoryProfile -> Flow API.DirectoryDriverListItem
buildDriverListItem profile = do
  person <- QP.findById profile.driverId >>= fromMaybeM (PersonNotFound ("No person found with id " <> show profile.driverId))
  mbDriverStats <- QDS.findByPrimaryKey profile.driverId
  mbVehicle <- QVeh.findById profile.driverId
  pure
    API.DirectoryDriverListItem
      { driverId = profile.driverId,
        name = person.firstName <> maybe "" (" " <>) person.lastName,
        rating = mbDriverStats >>= (.rating),
        totalRides = (.totalRides) <$> mbDriverStats,
        yearsOfExperience = profile.yearsOfExperience,
        preferredVehicleType = profile.preferredVehicleType,
        city = Nothing,
        bio = profile.bio,
        listedAt = profile.listedAt,
        phoneNumber = if profile.showPhoneNumber then person.mobileNumber else Nothing,
        vehicleInfo = if profile.showVehicleInfo then (.model) =<< mbVehicle else Nothing
      }
