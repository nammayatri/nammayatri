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
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.HashMap.Strict as HashMap
import Data.Text as Text
import Domain.Types.Merchant.DriverPoolConfig
import Domain.Types.Merchant.DriverPoolConfig as DPC
import Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import Kernel.Utils.Error
import Kernel.Utils.Logging
import qualified Storage.CachedQueries.Merchant.DriverPoolConfig as CDP

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
getDriverPoolConfig merchantOpCityId mbvt dist = do
  dpcCond <- liftIO $ CM.hashMapToString HashMap.fromList $ [(pack "merchantOperatingCityId", DA.String (Text.pack ("favorit0-0000-0000-0000-00000favorit"))), (pack "tripDistance", DA.String (Text.pack (dist)))] <> (bool [] [(pack "variant", DA.String (Text.pack (dist)))] (isJust mbvt))
  contextValue <- liftIO $ CM.evalCtx "test" dpcCond
  logDebug $ "the fetched context value is " <> show contextValue
  value <- liftIO $ (CM.hashMapToString (fromMaybe (HashMap.fromList [(pack "defaultKey", DA.String (Text.pack ("defaultValue")))]) contextValue))
  return $ buildDpcType (fromMaybe value contextValue)
  where
    buildDpcType cv =
      DPC.DriverPoolConfig
        { id = fromMaybe "" $ BL.unpack . encode <$> (HashMap.lookup "id" cv),
          merchantId = fromMaybe "" $ BL.unpack . encode <$> (HashMap.lookup "merchantId" cv),
          merchantOperatingCityId = fromMaybe "" $ BL.unpack . encode <$> (HashMap.lookup "merchantOperatingCityId" cv),
          minRadiusOfSearch = fromMaybe "" $ BL.unpack . encode <$> (HashMap.lookup "minRadiusOfSearch" cv),
          maxRadiusOfSearch = fromMaybe "" $ BL.unpack . encode <$> (HashMap.lookup "maxRadiusOfSearch" cv),
          radiusStepSize = fromMaybe "" $ BL.unpack . encode <$> (HashMap.lookup "radiusStepSize" cv),
          driverPositionInfoExpiry = fromMaybe "" $ BL.unpack . encode <$> (HashMap.lookup "driverPositionInfoExpiry" cv),
          actualDistanceThreshold = fromMaybe "" $ BL.unpack . encode <$> (HashMap.lookup "actualDistanceThreshold" cv),
          maxDriverQuotesRequired = fromMaybe "" $ BL.unpack . encode <$> (HashMap.lookup "maxDriverQuotesRequired" cv),
          driverQuoteLimit = fromMaybe "" $ BL.unpack . encode <$> (HashMap.lookup "driverQuoteLimit" cv),
          driverRequestCountLimit = fromMaybe "" $ BL.unpack . encode <$> (HashMap.lookup "driverRequestCountLimit" cv),
          driverBatchSize = fromMaybe "" $ BL.unpack . encode <$> (HashMap.lookup "driverBatchSize" cv),
          distanceBasedBatchSplit = fromMaybe "" $ BL.unpack . encode <$> (HashMap.lookup "distanceBasedBatchSplit" cv),
          maxNumberOfBatches = fromMaybe "" $ BL.unpack . encode <$> (HashMap.lookup "maxNumberOfBatches" cv),
          maxParallelSearchRequests = fromMaybe "" $ BL.unpack . encode <$> (HashMap.lookup "maxParallelSearchRequests" cv),
          poolSortingType = fromMaybe "" $ BL.unpack . encode <$> (HashMap.lookup "poolSortingType" cv),
          singleBatchProcessTime = fromMaybe "" $ BL.unpack . encode <$> (HashMap.lookup "singleBatchProcessTime" cv),
          tripDistance = fromMaybe "" $ BL.unpack . encode <$> (HashMap.lookup "tripDistance" cv),
          radiusShrinkValueForDriversOnRide = fromMaybe "" $ BL.unpack . encode <$> (HashMap.lookup "radiusShrinkValueForDriversOnRide" cv),
          driverToDestinationDistanceThreshold = fromMaybe "" $ BL.unpack . encode <$> (HashMap.lookup "driverToDestinationDistanceThreshold" cv),
          driverToDestinationDuration = fromMaybe "" $ BL.unpack . encode <$> (HashMap.lookup "driverToDestinationDuration" cv),
          createdAt = fromMaybe "" $ BL.unpack . encode <$> (HashMap.lookup "createdAt" cv),
          updatedAt = fromMaybe "" $ BL.unpack . encode <$> (HashMap.lookup "updatedAt" cv),
          vehicleVariant = fromMaybe "" $ BL.unpack . encode <$> (HashMap.lookup "vehicleVariant" cv)
        }
