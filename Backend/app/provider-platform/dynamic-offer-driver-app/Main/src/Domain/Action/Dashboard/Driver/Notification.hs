{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Driver.Notification
  ( sendDummyNotificationToDriver,
  )
where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Driver as Common
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.SearchRequest.SearchReqLocation as DSSL
import qualified Domain.Types.SearchRequestForDriver as DSearchReq
import qualified Domain.Types.Vehicle as DVeh
import Environment
import Kernel.Beam.Functions as B
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Notifications as TN

--------------------------------------------------------------------------------------------------

sendDummyNotificationToDriver :: ShortId DM.Merchant -> Context.City -> Id Common.Driver -> Flow APISuccess
sendDummyNotificationToDriver merchantShortId opCity driverId = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId opCity >>= fromMaybeM (MerchantOperatingCityDoesNotExist $ "merchantShortId: " <> merchantShortId.getShortId <> ", opCity: " <> show opCity)

  let personId = cast @Common.Driver @DP.Person driverId
  driver <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  -- merchant access check
  unless (merchantOperatingCity.id == driver.merchantOperatingCityId) $ throwError (PersonDoesNotExist personId.getId)

  now <- getCurrentTime
  let deviceToken = driver.deviceToken
      entityData = mkDummyNotificationEntityData now
  void $ TN.notifyOnNewSearchRequestAvailable merchantOperatingCity.id driver.id deviceToken entityData
  pure Success

dummyId :: Text
dummyId = ""

mkDummyNotificationEntityData :: UTCTime -> DSearchReq.SearchRequestForDriverAPIEntity
mkDummyNotificationEntityData now =
  let searchRequestValidTill = addUTCTime 30 now
      fromLocation = mkDummySearchReqFromLocation now
      toLocation = mkDummySearchReqToLocation now
      newFromLocation = mkDummyFromLocation now
      newToLocation = mkDummyToLocation now
   in DSearchReq.SearchRequestForDriverAPIEntity
        { searchRequestId = Id dummyId,
          searchTryId = Id dummyId,
          startTime = now,
          distance = Meters 713,
          distanceToPickup = Meters 149,
          durationToPickup = Seconds 65,
          baseFare = Money 40,
          driverLatLong = LatLong {lat = 12.9421783, lon = 77.62205},
          driverMinExtraFee = Just (Money 0),
          driverMaxExtraFee = Just (Money 20),
          rideRequestPopupDelayDuration = Seconds 0,
          keepHiddenForSeconds = Seconds 0,
          requestedVehicleVariant = DVeh.AUTO_RICKSHAW,
          bapName = Nothing,
          bapLogo = Nothing,
          customerExtraFee = Nothing,
          specialLocationTag = Nothing,
          disabilityTag = Nothing,
          goHomeRequestId = Nothing,
          isTranslated = False,
          ..
        }

mkDummySearchReqFromLocation :: UTCTime -> DSSL.SearchReqLocation
mkDummySearchReqFromLocation now =
  let DLoc.LocationAddress {..} = mkDummyFromAddress
   in DSSL.SearchReqLocation
        { id = Id dummyId,
          lat = 12.9421783,
          lon = 77.62205,
          full_address = fullAddress,
          createdAt = now,
          updatedAt = now,
          ..
        }

mkDummySearchReqToLocation :: UTCTime -> DSSL.SearchReqLocation
mkDummySearchReqToLocation now =
  let DLoc.LocationAddress {..} = mkDummyToAddress
   in DSSL.SearchReqLocation
        { id = Id dummyId,
          lat = 12.938797,
          lon = 77.624116,
          full_address = fullAddress,
          createdAt = now,
          updatedAt = now,
          ..
        }

mkDummyFromLocation :: UTCTime -> DLoc.Location
mkDummyFromLocation now =
  DLoc.Location
    { id = Id dummyId,
      address = mkDummyFromAddress,
      lat = 12.94217,
      lon = 77.62205,
      createdAt = now,
      updatedAt = now
    }

mkDummyToLocation :: UTCTime -> DLoc.Location
mkDummyToLocation now =
  DLoc.Location
    { id = Id dummyId,
      address = mkDummyToAddress,
      lat = 12.938797,
      lon = 77.624116,
      createdAt = now,
      updatedAt = now
    }

mkDummyFromAddress :: DLoc.LocationAddress
mkDummyFromAddress =
  DLoc.LocationAddress
    { door = Just "817",
      building = Just "20th Main Rd",
      street = Just "Koramangala 8th Block",
      area = Just "Koramangala, Koramangala 8th Block, 20th Main Rd",
      areaCode = Just "560095",
      city = Just "Bengaluru",
      state = Just "Karnataka 560095",
      country = Just "India",
      fullAddress = Just "817, 20th Main Rd, Koramangala 8th Block, Koramangala, Bengaluru, Karnataka 560095, 560095, India"
    }

mkDummyToAddress :: DLoc.LocationAddress
mkDummyToAddress =
  DLoc.LocationAddress
    { door = Just "831",
      building = Just "17th F Main Rd",
      street = Just "6th Block",
      area = Just "Koramangala, 6th Block",
      areaCode = Just "560095",
      city = Just "Bengaluru",
      state = Just "Karnataka 560095",
      country = Just "India",
      fullAddress = Just "Rohit 17th F Main Rd, 6th Block, Koramangala, Bengaluru, Karnataka 560095, 560095, India"
    }

--------------------------------------------------------------------------------------------------
