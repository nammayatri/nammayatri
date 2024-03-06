module Domain.Types.CacType where

-- import qualified Domain.Types.DriverPoolConfig as DPC

-- import qualified Kernel.Types.Common as KTC
-- import qualified Kernel.Types.Id as KTI
-- import qualified Domain.Types.Merchant.MerchantOperatingCity as DTM
import qualified Data.Aeson as A
import Data.Text as Text
import Domain.Types.Merchant
import Domain.Types.Merchant.MerchantOperatingCity
import Domain.Types.Vehicle.Variant
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config
import qualified Storage.Beam.DriverPoolConfig as DPC

checkParseCommon :: (String, A.Value) -> String
checkParseCommon (key, value) = do
  case Text.splitOn ":" (pack key) of
    [tableName, tableColumn] -> do
      case tableName of
        "driverPoolConfig" -> show $ checkParse (Proxy @DPC.DriverPoolConfig) tableColumn value
        _ -> "This is to already not there no parsing req"
    _ -> "Bro what the f**k you should follow a format."

class CheckParse table where
  checkParse :: Proxy table -> Text -> A.Value -> Bool

checkField :: forall a. A.FromJSON a => Proxy a -> A.Value -> Bool
checkField _ value = case A.fromJSON @a value of
  A.Success _ -> True
  A.Error _ -> False

-- type family TransporterConfigT Symbol
--  where
--     TransporterConfigT "actualDistanceThreshold" = Meters

instance CheckParse DPC.DriverPoolConfig where
  checkParse _ tableColumn value =
    case tableColumn of
      "actualDistanceThreshold" -> checkField (Proxy @Meters) value
      "createdAt" -> checkField (Proxy @UTCTime) value
      "distanceBasedBatchSplit" -> checkField (Proxy @[BatchSplitByPickupDistance]) value
      "driverBatchSize" -> checkField (Proxy @Int) value
      "driverPositionInfoExpiry" -> checkField (Proxy @Seconds) value
      "driverQuoteLimit" -> checkField (Proxy @Int) value
      "driverRequestCountLimit" -> checkField (Proxy @Int) value
      "driverToDestinationDistanceThreshold" -> checkField (Proxy @Meters) value
      "driverToDestinationDuration" -> checkField (Proxy @Seconds) value
      "id" -> checkField (Proxy @(Id DPC.DriverPoolConfig)) value
      "maxDriverQuotesRequired" -> checkField (Proxy @Int) value
      "maxNumberOfBatches" -> checkField (Proxy @Int) value
      "maxParallelSearchRequests" -> checkField (Proxy @Int) value
      "maxRadiusOfSearch" -> checkField (Proxy @Meters) value
      "merchantId" -> checkField (Proxy @(Id Merchant)) value
      "merchantOperatingCityId" -> checkField (Proxy @(Id MerchantOperatingCity)) value
      "minRadiusOfSearch" -> checkField (Proxy @Meters) value
      "poolSortingType" -> checkField (Proxy @PoolSortingType) value
      "radiusShrinkValueForDriversOnRide" -> checkField (Proxy @Meters) value
      "radiusStepSize" -> checkField (Proxy @Meters) value
      "scheduleTryTimes" -> checkField (Proxy @[Int]) value
      "singleBatchProcessTime" -> checkField (Proxy @Seconds) value
      "thresholdToIgnoreActualDistanceThreshold" -> checkField (Proxy @(Maybe Meters)) value
      "tripCategory" -> checkField (Proxy @Text) value
      "tripDistance" -> checkField (Proxy @Meters) value
      "updatedAt" -> checkField (Proxy @UTCTime) value
      "vehicleVariant" -> checkField (Proxy @(Maybe Variant)) value
      _ -> True
