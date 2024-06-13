{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.KnowYourDriver where

import qualified Domain.Action.UI.Person as SP
import Domain.Types.DriverModuleCompletion
import qualified Domain.Types.Person as DP
import Domain.Types.Ride
import Environment
import Kernel.Beam.Functions
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Error
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.DriverModuleCompletion as SQDMC
import qualified Storage.Queries.DriverProfileQuestions as DPQ
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.LmsModule as QLmsModule
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error

data DriverProfileRes = DriverProfileRes
  { certificates :: [Text],
    driverName :: Text,
    likedByRidersNum :: Int,
    trips :: Int,
    approvalRate :: Maybe Centesimal,
    cancellation :: Int,
    onboardedAt :: UTCTime,
    pledges :: [Text],
    expertAt :: [Text],
    whyNY :: [Text],
    languages :: [Text]
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

knowYourDriver :: Id Ride -> Maybe Text -> Flow DriverProfileRes
knowYourDriver rideId apiKey = do
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  merchantId <- maybe ((runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)) <&> (.providerId)) return ride.merchantId
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  person <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  getDriverProfile person

knowYourFavDriver :: Id DP.Person -> Maybe Text -> Flow DriverProfileRes
knowYourFavDriver driverId apiKey = do
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  merchant <- QM.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  getDriverProfile person

getDriverProfile :: DP.Person -> Flow DriverProfileRes
getDriverProfile person = do
  driverProfileQues <- DPQ.findByPersonId person.id >>= fromMaybeM (InternalError $ "No driver profile against driver id : " <> person.id.getId)
  driverStats <- B.runInReplica $ QDriverStats.findById (cast person.id) >>= fromMaybeM (PersonNotFound person.id.getId)
  driverModulesCompleted <- SQDMC.findByDriverIdAndStatus person.id MODULE_COMPLETED
  modules <- mapM (\driverModule -> QLmsModule.findById driverModule.moduleId) driverModulesCompleted
  let moduleCategories = mapMaybe (fmap (.category)) modules
      moduleCategoriesText = map show moduleCategories
  pure $
    DriverProfileRes
      { certificates = moduleCategoriesText,
        driverName = person.firstName,
        likedByRidersNum = driverStats.favRiderCount,
        trips = driverStats.totalRides,
        approvalRate = SP.roundToOneDecimal <$> driverStats.rating,
        cancellation = div ((fromMaybe 0 driverStats.ridesCancelled) * 100 :: Int) (nonZero driverStats.totalRidesAssigned :: Int),
        onboardedAt = person.createdAt,
        pledges = driverProfileQues.pledges,
        expertAt = driverProfileQues.expertAt,
        whyNY = driverProfileQues.whyNY,
        languages = fromMaybe [] person.languagesSpoken
      }
  where
    nonZero Nothing = 1
    nonZero (Just a)
      | a <= 0 = 1
      | otherwise = a
