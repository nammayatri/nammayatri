{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.BehaviourManagement.GpsTollBehavior
  ( GpsTollBehaviorData (..),
    GpsTollBehaviorOutput (..),
    mkGpsTollBadBehaviorKey,
    incrementGpsTollBadBehaviorCount,
    getGpsTollBadBehaviorCount,
    evaluateGpsTollBehavior,
    blockDriverForTollRoutes,
  )
where

import qualified Data.Aeson as A
import Data.Default.Class
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config
import qualified Kernel.Storage.Hedis as Redis
-- import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Kernel.Types.SlidingWindowCounters as SWC
import Kernel.Utils.Common
import qualified Kernel.Utils.SlidingWindowCounters as SWC
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import qualified Storage.Queries.DriverInformation as QDriverInformation

-- | Input data for GPS toll behavior evaluation
-- Contains toll information from estimate and ride, plus historical behavior
data GpsTollBehaviorData = GpsTollBehaviorData
  { -- Estimated toll info (from booking/estimate)
    estimatedTollCharges :: Maybe HighPrecMoney,
    estimatedTollNames :: Maybe [Text],
    estimatedTollIds :: Maybe [Text],
    -- Detected toll info (from ride)
    detectedTollCharges :: Maybe HighPrecMoney,
    detectedTollNames :: Maybe [Text],
    detectedTollIds :: Maybe [Text],
    -- GPS behavior in current ride
    gpsTurnedOffInCurrentRide :: Bool,
    -- Historical bad behavior count (from sliding window)
    badBehaviorInTollRouteCount :: Integer
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Default GpsTollBehaviorData where
  def =
    GpsTollBehaviorData
      { estimatedTollCharges = Nothing,
        estimatedTollNames = Nothing,
        estimatedTollIds = Nothing,
        detectedTollCharges = Nothing,
        detectedTollNames = Nothing,
        detectedTollIds = Nothing,
        gpsTurnedOffInCurrentRide = False,
        badBehaviorInTollRouteCount = 0
      }

-- | Output from GPS toll behavior evaluation
data GpsTollBehaviorOutput = GpsTollBehaviorOutput
  { shouldBlockForFutureTollRide :: Bool,
    shouldIncrementBadBehaviorTollCounter :: Bool,
    -- How many hours to block the driver for toll routes (0 = no block)
    blockDurationInHours :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Default GpsTollBehaviorOutput where
  def =
    GpsTollBehaviorOutput
      { shouldBlockForFutureTollRide = False,
        shouldIncrementBadBehaviorTollCounter = False,
        blockDurationInHours = 0
      }

-- | Redis key for GPS toll bad behavior sliding window counter
-- Window size: 30 days (stores data for 30 days, but can query for fewer days)
mkGpsTollBadBehaviorKey :: Text -> Text
mkGpsTollBadBehaviorKey driverId = "driver-offer:gps-toll-bad-behavior:counter-dId:" <> driverId

-- | Increment the GPS toll bad behavior counter
-- Uses a 30-day sliding window for storage
incrementGpsTollBadBehaviorCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DP.Person ->
  m ()
incrementGpsTollBadBehaviorCount driverId =
  Redis.withCrossAppRedis $
    SWC.incrementWindowCount
      (mkGpsTollBadBehaviorKey driverId.getId)
      (SWC.SlidingWindowOptions 30 SWC.Days) -- 30 day window for storage

-- | Get the GPS toll bad behavior count for specified number of days
-- windowDays: how many days of data to consider (from transporter config)
-- The sliding window stores 30 days of data, but we can query for fewer days
getGpsTollBadBehaviorCount ::
  ( Redis.HedisFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Int -> -- windowDays: how many days to consider
  Id DP.Person ->
  m Integer
getGpsTollBadBehaviorCount windowDays driverId =
  Redis.withCrossAppRedis $ do
    -- Get values for the last 'windowDays' days from a 30-day sliding window
    values <- SWC.getCurrentWindowValuesUptoLast (toInteger windowDays) (mkGpsTollBadBehaviorKey driverId.getId) (SWC.SlidingWindowOptions 30 SWC.Days)
    return $ sum $ map (fromMaybe 0) values

-- | Evaluate GPS toll behavior using App Dynamic Logic rules
-- Rules should be fetched from GPS_TOLL_BEHAVIOR domain by the caller
evaluateGpsTollBehavior ::
  ( MonadFlow m,
    ClickhouseFlow m r,
    CacheFlow m r
  ) =>
  Id LYT.MerchantOperatingCity ->
  LYT.LogicDomain ->
  [A.Value] -> -- Rules from App Dynamic Logic (GPS_TOLL_BEHAVIOR domain)
  GpsTollBehaviorData ->
  m GpsTollBehaviorOutput
evaluateGpsTollBehavior mocId domain allLogics inputData = do
  if null allLogics
    then do
      logInfo "No GPS toll behavior rules configured in App Dynamic Logic, using default output"
      return def
    else do
      logDebug $ "Evaluating GPS toll behavior with input: " <> show inputData
      resp <- withTryCatch "runLogics:GpsTollBehavior" $ LYDL.runLogicsWithDebugLog LYDL.Driver mocId domain allLogics inputData
      case resp of
        Left err -> do
          logError $ "Error in running GPS toll behavior logics - " <> show err <> " - " <> show inputData
          return def
        Right resp' -> parseLogicOutput resp'

-- | Parse the logic output into GpsTollBehaviorOutput
parseLogicOutput :: (MonadFlow m) => LYT.RunLogicResp -> m GpsTollBehaviorOutput
parseLogicOutput resp = do
  if not (null resp.errors)
    then do
      logError $ "Errors in GPS toll behavior logic evaluation: " <> show resp.errors
      return def
    else do
      case A.fromJSON resp.result of
        A.Success output -> return output
        A.Error err -> do
          logError $ "Failed to parse GPS toll behavior output: " <> show err <> ", result: " <> show resp.result
          return def

-- | Block driver for toll routes for the specified duration
blockDriverForTollRoutes ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DP.Person ->
  Int -> -- Block duration in hours
  m ()
blockDriverForTollRoutes driverId blockDurationHours = do
  when (blockDurationHours > 0) $ do
    now <- getCurrentTime
    let blockUntil = addUTCTime (fromIntegral blockDurationHours * 3600) now
    logInfo $ "Blocking driver " <> driverId.getId <> " for toll routes until: " <> show blockUntil
    QDriverInformation.updateTollRouteBlockedTill (Just blockUntil) (cast driverId)
