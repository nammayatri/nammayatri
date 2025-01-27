{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.RideSummary where

import Data.Time (Day)
import Domain.Types.DailyStats as DDS
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DM
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.DailyStats as SQDS

data DriverRideSummaryResp = DriverRideSummaryResp
  { earnings :: Money,
    earningsWithCurrency :: PriceAPIEntity,
    rideDistance :: Meters,
    rideDistanceWithUnit :: Distance,
    rideDate :: Day,
    noOfRides :: Int,
    tipAmount :: PriceAPIEntity,
    cancellationCharges :: PriceAPIEntity,
    rideDuration :: Seconds
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype DriverRideSummaryListResp = DriverRideSummaryListResp
  { list :: [DriverRideSummaryResp]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

listDailyRidesSummary :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Id Merchant.Merchant -> Id DM.MerchantOperatingCity -> [Day] -> m DriverRideSummaryListResp
listDailyRidesSummary driverId _ _ req = do
  dailyStatsList <- mapM (SQDS.findByDriverIdAndDate driverId) req
  let list = mkRideSummaryList (catMaybes dailyStatsList)
  return $ DriverRideSummaryListResp {..}

mkRideSummaryList :: [DDS.DailyStats] -> [DriverRideSummaryResp]
mkRideSummaryList =
  map
    ( \x ->
        DriverRideSummaryResp
          { earnings = roundToIntegral x.totalEarnings,
            earningsWithCurrency = PriceAPIEntity x.totalEarnings x.currency,
            rideDistance = x.totalDistance,
            rideDistanceWithUnit = convertMetersToDistance x.distanceUnit x.totalDistance,
            rideDate = x.merchantLocalDate,
            noOfRides = x.numRides,
            tipAmount = PriceAPIEntity x.tipAmount x.currency,
            cancellationCharges = PriceAPIEntity x.cancellationCharges x.currency,
            rideDuration = x.totalRideTime
          }
    )
