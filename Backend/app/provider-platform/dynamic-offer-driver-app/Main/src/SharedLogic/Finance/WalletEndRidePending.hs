{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | End-ride finance jobs enqueued but not yet completed, per city, scored by enqueue time. The
--   wallet integrity job re-enqueues anything left here too long (the job failed past its retry
--   budget), so a ride's wallet postings are never lost.
module SharedLogic.Finance.WalletEndRidePending
  ( markEndRideFinancePending,
    clearEndRideFinancePending,
    findStaleEndRideFinance,
  )
where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common

pendingEndRideKey :: Id MerchantOperatingCity -> Text
pendingEndRideKey cityId = "WalletEndRidePending:" <> cityId.getId

pendingMember :: Text -> Text -> Text
pendingMember rideId bookingId = rideId <> ":" <> bookingId

markEndRideFinancePending :: (MonadFlow m, Redis.HedisFlow m r) => Id MerchantOperatingCity -> Text -> Text -> m ()
markEndRideFinancePending cityId rideId bookingId = do
  now <- getCurrentTime
  void $ Redis.zAddExp (pendingEndRideKey cityId) (pendingMember rideId bookingId) (round $ utcToMilliseconds now) (7 * 24 * 3600)

clearEndRideFinancePending :: (MonadFlow m, Redis.HedisFlow m r) => Id MerchantOperatingCity -> Text -> Text -> m ()
clearEndRideFinancePending cityId rideId bookingId =
  void $ Redis.zRem (pendingEndRideKey cityId) [pendingMember rideId bookingId]

-- | (rideId, bookingId) of end-ride finance jobs pending since before the cutoff.
findStaleEndRideFinance :: (MonadFlow m, Redis.HedisFlow m r) => Id MerchantOperatingCity -> UTCTime -> m [(Text, Text)]
findStaleEndRideFinance cityId cutoff = do
  members <- Redis.zRangeByScore (pendingEndRideKey cityId) 0 (utcToMilliseconds cutoff)
  pure $ mapMaybe (parse . TE.decodeUtf8) members
  where
    parse member = case T.splitOn ":" member of
      [rideId, bookingId] -> Just (rideId, bookingId)
      _ -> Nothing
