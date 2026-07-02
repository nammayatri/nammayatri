{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.ActiveDriversList
  ( mkActiveDriversSetKey,
    mkActiveDriversSetKeyForShard,
    getActiveDriversForShard,
    getShardNumsForFanOut,
    addDriverToActiveList,
    localDateStr,
  )
where

import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Data.Text as Text
import EulerHS.Prelude
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Domain.Types.Person as DP

formatDate :: Day -> Text
formatDate day =
  let (year, month, dayOfMonth) = toGregorian day
   in Text.pack $ show year <> "-" <> padZero month <> "-" <> padZero dayOfMonth
  where
    padZero n = if n < 10 then "0" <> show n else show n

localDateStr :: UTCTime -> NominalDiffTime -> Text
localDateStr referenceTime diffTimeFromUTC = formatDate (utctDay (addUTCTime diffTimeFromUTC referenceTime))

mkActiveDriversSetKey :: MonadTime m => Int -> Text -> NominalDiffTime -> m Text
mkActiveDriversSetKey shards driverId diffTimeFromUTC = do
  now <- getCurrentTime
  let localTime = addUTCTime diffTimeFromUTC now
  let dateStr = formatDate (utctDay localTime)
  pure $ "ActiveDrivers:Set:" <> dateStr <> ":" <> Redis.shardHashTag shards driverId

mkActiveDriversSetKeyForShard :: Int -> UTCTime -> NominalDiffTime -> Text
mkActiveDriversSetKeyForShard shardNum referenceTime diffTimeFromUTC =
  "ActiveDrivers:Set:" <> localDateStr referenceTime diffTimeFromUTC <> ":{shard-" <> show shardNum <> "}"

getActiveDriversForShard :: Redis.HedisFlow m r => Int -> UTCTime -> NominalDiffTime -> m [Text]
getActiveDriversForShard shardNum referenceTime diffTimeFromUTC =
  Redis.sMembers (mkActiveDriversSetKeyForShard shardNum referenceTime diffTimeFromUTC)

getShardNumsForFanOut :: forall r m. (MonadReader r m, HasField "activeDriversListKeyShards" r Int) => m [Maybe Int]
getShardNumsForFanOut = do
  n <- asks @r (.activeDriversListKeyShards)
  pure $ if n <= 0 then [Nothing] else Just <$> [0 .. n - 1]

getTTLForActiveDriversList :: MonadTime m => NominalDiffTime -> m Int
getTTLForActiveDriversList diffTimeFromUTC = do
  now <- getCurrentTime
  let localTime = addUTCTime diffTimeFromUTC now
  let localDate = utctDay localTime
  let tomorrowLocalDate = addDays 1 localDate
  let tomorrowLocalEnd = UTCTime tomorrowLocalDate (secondsToDiffTime 0)
  let tomorrowUTC = addUTCTime (negate diffTimeFromUTC) tomorrowLocalEnd
  pure $ ceiling (realToFrac (diffUTCTime tomorrowUTC now) :: Double)

addDriverToActiveList ::
  forall r m.
  (MonadThrow m, Log m, MonadTime m, Redis.HedisFlow m r, HasField "activeDriversListKeyShards" r Int) =>
  Id DP.Person ->
  NominalDiffTime ->
  m ()
addDriverToActiveList driverId diffTimeFromUTC = do
  shards <- asks @r (.activeDriversListKeyShards)
  ttl <- getTTLForActiveDriversList diffTimeFromUTC
  key <- mkActiveDriversSetKey shards (driverId.getId) diffTimeFromUTC
  void $ Redis.sAddExp key [driverId.getId] ttl
