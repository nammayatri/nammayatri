{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.DriverCoins.CoinBalance
  ( getCoinsByDriverId,
    getExpirationSeconds,
    mkCoinAccumulationByDriverIdKey,
    getCoinAccumulationByDriverIdKey,
    setCoinAccumulationByDriverIdKey,
  )
where

import Data.Time (UTCTime (UTCTime, utctDay), addDays)
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Coins.CoinHistory as CHistory

getCoinsByDriverId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Seconds -> m Int
getCoinsByDriverId driverId timeDiffFromUtc = Hedis.withLockRedisAndReturnValue driverId.getId 60 $ do
  now <- getCurrentTime
  let istTime = addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) now
  let currentDate = show $ utctDay istTime
  expirationPeriod <- getExpirationSeconds timeDiffFromUtc
  coinKeyExists <- getCoinAccumulationByDriverIdKey driverId currentDate
  case coinKeyExists of
    Just coinBalance -> pure coinBalance
    Nothing -> do
      totalCoins <- CHistory.getTotalCoins driverId (secondsToNominalDiffTime timeDiffFromUtc)
      let coinBalance = sum $ map (\coinHistory -> coinHistory.coins - coinHistory.coinsUsed) totalCoins
      Hedis.whenWithLockRedis (mkCoinAccumulationByDriverIdKey driverId currentDate) 60 $ do
        setCoinAccumulationByDriverIdKey driverId currentDate coinBalance expirationPeriod
      pure coinBalance

getExpirationSeconds :: MonadFlow m => Seconds -> m Int
getExpirationSeconds timeDiffFromUtc = do
  now <- getCurrentTime
  let istTime = addUTCTime (secondsToNominalDiffTime timeDiffFromUtc) now
  let expirationSeconds = round $ diffUTCTime (UTCTime (addDays 1 $ utctDay istTime) 0) istTime
  pure expirationSeconds

mkCoinAccumulationByDriverIdKey :: Id DP.Person -> Text -> Text
mkCoinAccumulationByDriverIdKey driverId date = "DriverCoinBalance:DriverId:" <> driverId.getId <> ":" <> date

getCoinAccumulationByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Text -> m (Maybe Int)
getCoinAccumulationByDriverIdKey driverId currentDate = Hedis.withCrossAppRedis $ Hedis.get (mkCoinAccumulationByDriverIdKey driverId currentDate)

setCoinAccumulationByDriverIdKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DP.Person -> Text -> Int -> Int -> m ()
setCoinAccumulationByDriverIdKey driverId currentDate count expirationPeriod = do
  void $ Hedis.withCrossAppRedis $ Hedis.incrby (mkCoinAccumulationByDriverIdKey driverId currentDate) (fromIntegral count)
  Hedis.withCrossAppRedis $ Hedis.expire (mkCoinAccumulationByDriverIdKey driverId currentDate) expirationPeriod
