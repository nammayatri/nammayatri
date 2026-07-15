{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Production 'RideRegistry' over Redis (@session/ride-registry.ts@): the durable
-- index of bookings the background tracker watches. @claimStage@ is an atomic
-- claim-once (Redis @SET NX EX@) so exactly one tick pushes each (booking,stage)
-- update; @releaseStage@ undoes it on a failed send. Ride blobs + the index sets
-- carry a @max(60, trackMaxAge)@ TTL so a ride outlives the 30-min session but
-- can't leak forever.
module WhatsappBot.Adapter.Registry (mkRideRegistry) where

import Environment (Flow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import WhatsappBot.Handles (RegisteredRide (..), RideRegistry (..))

ridesSetKey :: Text
ridesSetKey = "wab:activerides"

rideKey :: Text -> Text
rideKey bid = "wab:activeride:" <> bid

userSetKey :: Text -> Text
userSetKey uk = "wab:activerides:user:" <> uk

claimKey :: Text -> Text -> Text
claimKey bid stage = "wab:activeride:sent:" <> bid <> ":" <> stage

-- | @trackMaxAgeSec@ = metaTrackerMaxAgeSec (ride index TTL floor 60s).
mkRideRegistry :: Int -> RideRegistry Flow
mkRideRegistry trackMaxAgeSec =
  RideRegistry
    { registerRide = \r -> do
        Redis.sAddExp ridesSetKey [r.bookingId] ttl
        Redis.setExp (rideKey r.bookingId) r ttl
        Redis.sAddExp (userSetKey r.userKey) [r.bookingId] ttl,
      getRide = \bid -> Redis.get (rideKey bid),
      removeRide = \bid -> do
        mRide <- Redis.get (rideKey bid)
        whenJust mRide $ \(r :: RegisteredRide) -> void $ Redis.srem (userSetKey r.userKey) [bid]
        void $ Redis.srem ridesSetKey [bid]
        Redis.del (rideKey bid),
      updateRide = \r -> do
        -- Refresh the blob AND both index sets' TTL (TS update()): otherwise a
        -- long-running ride silently drops out of the index after ttl from
        -- registration, even though the tracker keeps touching it.
        Redis.setExp (rideKey r.bookingId) r ttl
        Redis.sAddExp ridesSetKey [r.bookingId] ttl
        Redis.sAddExp (userSetKey r.userKey) [r.bookingId] ttl,
      claimStage = \bid stage -> do
        won <- Redis.setNxExpire (claimKey bid stage) ttl True
        if won
          then pure True
          else do
            -- setNxExpire == False is ambiguous: a genuine prior claim OR a
            -- swallowed Redis error. Confirm — a present key is a real duplicate
            -- (skip), but its ABSENCE means the write errored, so return True to
            -- proceed with the send rather than silently finalize-and-drop the
            -- ride's terminal (ended/cancelled) message.
            mExisting <- Redis.get (claimKey bid stage) :: Flow (Maybe Bool)
            pure (isNothing mExisting),
      releaseStage = \bid stage -> Redis.del (claimKey bid stage),
      listRides = ridesFor ridesSetKey (const True),
      listByUser = \uk -> ridesFor (userSetKey uk) (\r -> r.userKey == uk),
      hasActiveRide = \uk -> not . null <$> ridesFor (userSetKey uk) (\r -> r.userKey == uk)
    }
  where
    ttl = max 60 trackMaxAgeSec
    -- Enumerate a bookingId set and hydrate the live ride blobs (a blob may have
    -- expired while still in the set — skip it; the mock keys off the ride map too).
    ridesFor :: Text -> (RegisteredRide -> Bool) -> Flow [RegisteredRide]
    ridesFor setKey keep = do
      bids <- Redis.sMembers setKey
      rides <- catMaybes <$> mapM (Redis.get . rideKey) bids
      pure (filter keep rides)
