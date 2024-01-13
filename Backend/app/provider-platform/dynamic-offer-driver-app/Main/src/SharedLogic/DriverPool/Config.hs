{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverPool.Config where

import Client.Main as CM
import Data.Aeson as DA
import Data.HashMap.Strict as HashMap
import Data.Text as Text
import Domain.Types.Merchant.DriverPoolConfig
import Domain.Types.Merchant.DriverPoolConfig as DPC
import Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import Kernel.Utils.Logging

data CancellationScoreRelatedConfig = CancellationScoreRelatedConfig
  { popupDelayToAddAsPenalty :: Maybe Seconds,
    thresholdCancellationScore :: Maybe Int,
    minRidesForCancellationScore :: Maybe Int
  }
  deriving (Generic)

getDriverPoolConfig ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  Maybe Variant.Variant ->
  Meters ->
  m DriverPoolConfig
getDriverPoolConfig _ mbvt dist = do
  dpcCond <- liftIO $ CM.hashMapToString $ HashMap.fromList ([(pack "merchantOperatingCityId", DA.String (Text.pack ("favorit0-0000-0000-0000-00000favorit"))), (pack "tripDistance", DA.String (Text.pack (show dist)))] ++ (bool [] [(pack "variant", DA.String (Text.pack (show $ fromJust mbvt)))] (isJust mbvt)))
  contextValue <- liftIO $ CM.evalCtx "test" dpcCond
  logDebug $ "the fetched context value is " <> show contextValue
  --value <- liftIO $ (CM.hashMapToString (fromMaybe (HashMap.fromList [(pack "defaultKey", DA.String (Text.pack ("defaultValue")))]) contextValue))
  return $ buildDpcType (fromJust contextValue)
  where
    buildDpcType cv =
      DPC.DriverPoolConfig
        { id = (read . show) $ fromJust (HashMap.lookup "id" cv),
          merchantId = (read . show) $ fromJust (HashMap.lookup "merchantId" cv),
          merchantOperatingCityId = (read . show) $ fromJust (HashMap.lookup "merchantOperatingCityId" cv),
          minRadiusOfSearch = (read . show) $ fromJust (HashMap.lookup "minRadiusOfSearch" cv),
          maxRadiusOfSearch = (read . show) $ fromJust (HashMap.lookup "maxRadiusOfSearch" cv),
          radiusStepSize = (read . show) $ fromJust (HashMap.lookup "radiusStepSize" cv),
          driverPositionInfoExpiry = (read . show) $ fromJust (HashMap.lookup "driverPositionInfoExpiry" cv),
          actualDistanceThreshold = (read . show) $ fromJust (HashMap.lookup "actualDistanceThreshold" cv),
          maxDriverQuotesRequired = (read . show) $ fromJust (HashMap.lookup "maxDriverQuotesRequired" cv),
          driverQuoteLimit = (read . show) $ fromJust (HashMap.lookup "driverQuoteLimit" cv),
          driverRequestCountLimit = (read . show) $ fromJust (HashMap.lookup "driverRequestCountLimit" cv),
          driverBatchSize = (read . show) $ fromJust (HashMap.lookup "driverBatchSize" cv),
          distanceBasedBatchSplit = (read . show) $ fromJust (HashMap.lookup "distanceBasedBatchSplit" cv),
          maxNumberOfBatches = (read . show) $ fromJust (HashMap.lookup "maxNumberOfBatches" cv),
          maxParallelSearchRequests = (read . show) $ fromJust (HashMap.lookup "maxParallelSearchRequests" cv),
          poolSortingType = (read . show) $ fromJust (HashMap.lookup "poolSortingType" cv),
          singleBatchProcessTime = (read . show) $ fromJust (HashMap.lookup "singleBatchProcessTime" cv),
          tripDistance = (read . show) $ fromJust (HashMap.lookup "tripDistance" cv),
          radiusShrinkValueForDriversOnRide = (read . show) $ fromJust (HashMap.lookup "radiusShrinkValueForDriversOnRide" cv),
          driverToDestinationDistanceThreshold = (read . show) $ fromJust (HashMap.lookup "driverToDestinationDistanceThreshold" cv),
          driverToDestinationDuration = (read . show) $ fromJust (HashMap.lookup "driverToDestinationDuration" cv),
          createdAt = (read . show) $ fromJust (HashMap.lookup "createdAt" cv),
          updatedAt = (read . show) $ fromJust (HashMap.lookup "updatedAt" cv),
          vehicleVariant = (read . show) $ fromJust (HashMap.lookup "vehicleVariant" cv)
        }
