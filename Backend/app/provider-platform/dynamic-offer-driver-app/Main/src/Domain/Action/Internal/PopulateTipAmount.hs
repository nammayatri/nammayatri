{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.PopulateTipAmount where

import Data.Time
import Domain.Types.Ride
import Environment
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.DailyStats as QDailyStats
import qualified Storage.Queries.Ride as QRide
import Tools.Error

populateTipAmount :: Id Ride -> HighPrecMoney -> Maybe Text -> Flow APISuccess
populateTipAmount rideId tipAmount apiKey = do
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  merchantId <- maybe ((runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)) <&> (.providerId)) return ride.merchantId
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"

  QRide.updateTipAmountField (Just tipAmount) ride.id
  transporterConfig <- SCTC.findByMerchantOpCityId ride.merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound ride.merchantOperatingCityId.getId)
  localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  mbDailyStats <- QDailyStats.findByDriverIdAndDate ride.driverId (utctDay localTime)
  case mbDailyStats of
    Just stats -> QDailyStats.updateTipAmountByDriverId (stats.tipAmount + tipAmount) ride.driverId (utctDay localTime)
    Nothing -> logError $ "DailyStats not found during updation of tip amount for driverId : " <> show ride.driverId
  pure Success
