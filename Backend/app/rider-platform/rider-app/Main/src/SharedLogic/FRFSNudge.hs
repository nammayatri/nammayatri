{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.FRFSNudge (computeShuttleNudge) where

import qualified API.Types.UI.FRFSTicketService as APITypes
import qualified BecknV2.FRFS.Enums as Spec
import Data.List (nub)
import qualified Data.Time as T
import qualified Domain.Types.FRFSBookingGroup as DFRFSBookingGroup
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBookingStatus
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.FRFSBookingGroup as QFRFSBookingGroup
import qualified Storage.Queries.FRFSTicketBookingExtra as QFRFSTicketBookingExtra

-- | Decide whether a just-confirmed shuttle booking should carry a nudge, and which one:
--   * MULTI_SELECT_REPEAT: rider has >= 1 confirmed booking in this direction, and their total
--     confirmed bookings across both directions meet a configurable threshold. When this direction
--     is the reverse of the rider's primary (first-ever) direction, it's gated behind having
--     actually *completed* a real multi-select checkout in the primary direction -- not just been
--     shown the nudge for it.
--   * RETURN_TRIP: rider has exactly one confirmed booking ever, with nothing in the reverse
--     direction, and no established return-time pattern to match against.
-- Checked in that order: a misconfigured threshold of 1 could otherwise make both eligible on the
-- rider's very first booking, and multi-select should win that edge case.
computeShuttleNudge ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  DFRFSTicketBooking.FRFSTicketBooking ->
  m (Maybe APITypes.FRFSNudgeAPI)
computeShuttleNudge booking
  | booking.status /= DFRFSTicketBookingStatus.CONFIRMED = pure Nothing
  | booking.vehicleType /= Spec.BUS = pure Nothing
  | otherwise = do
    allConfirmed <- QFRFSTicketBookingExtra.findAllConfirmedByRiderIdAndVehicleType booking.riderId Spec.BUS
    mbRiderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing
    let threshold = fromMaybe 2 (mbRiderConfig >>= (.shuttleMultiSelectNudgeThreshold))
        timeDiffFromUtc = maybe (Seconds 19800) (.timeDiffFromUtc) mbRiderConfig
        direction = bookingDirection booking
        reverseDirection = swapDirection direction
        sameDirBookings = filter (\b -> bookingDirection b == direction) allConfirmed
        totalConfirmed = length allConfirmed
        primaryDirection = bookingDirection <$> listToMaybe allConfirmed
    mbMultiSelect <-
      if totalConfirmed >= threshold && not (null sameDirBookings)
        then
          if primaryDirection == Just reverseDirection
            then do
              let primaryDirBookings = filter (\b -> bookingDirection b == reverseDirection) allConfirmed
              accepted <- hasCompletedMultiSelectGroup primaryDirBookings
              pure $ if accepted then Just (mkMultiSelectNudge booking sameDirBookings) else Nothing
            else pure $ Just (mkMultiSelectNudge booking sameDirBookings)
        else pure Nothing
    case mbMultiSelect of
      Just nudge -> pure (Just nudge)
      Nothing ->
        pure $
          if totalConfirmed == 1
            then Just (mkReturnTripNudge booking timeDiffFromUtc)
            else Nothing
  where
    bookingDirection b = (b.fromStationCode, b.toStationCode)
    swapDirection (f, t) = (t, f)

-- | A real multi-select checkout, not merely a single-item cart that happened through the same
-- checkout endpoint -- distinguished by `totalSlots > 1` on the completed (CONFIRMED) booking group.
hasCompletedMultiSelectGroup :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [DFRFSTicketBooking.FRFSTicketBooking] -> m Bool
hasCompletedMultiSelectGroup bookings = do
  let groupIds = nub $ mapMaybe (.bookingGroupId) bookings
  groups <- catMaybes <$> mapM QFRFSBookingGroup.findById groupIds
  pure $ any (\g -> g.status == DFRFSBookingGroup.CONFIRMED && g.totalSlots > 1) groups

mkMultiSelectNudge :: DFRFSTicketBooking.FRFSTicketBooking -> [DFRFSTicketBooking.FRFSTicketBooking] -> APITypes.FRFSNudgeAPI
mkMultiSelectNudge booking sameDirBookings =
  APITypes.FRFSNudgeAPI
    { nudgeType = APITypes.MULTI_SELECT_REPEAT,
      fromStationCode = booking.fromStationCode,
      toStationCode = booking.toStationCode,
      timeWindowHint = Nothing,
      -- most recent same-direction confirmed booking (list is ascending by createdAt)
      seedBookingId = Just (last sameDirBookings).id
    }

mkReturnTripNudge :: DFRFSTicketBooking.FRFSTicketBooking -> Seconds -> APITypes.FRFSNudgeAPI
mkReturnTripNudge booking timeDiffFromUtc =
  APITypes.FRFSNudgeAPI
    { nudgeType = APITypes.RETURN_TRIP,
      fromStationCode = booking.toStationCode,
      toStationCode = booking.fromStationCode,
      timeWindowHint = oppositeWindow . bucketTimeWindow timeDiffFromUtc <$> booking.startTime,
      seedBookingId = Nothing
    }

bucketTimeWindow :: Seconds -> UTCTime -> APITypes.FRFSNudgeTimeWindow
bucketTimeWindow timeDiffFromUtc utcTime = windowForHour hourOfDay
  where
    localTime = addUTCTime (fromIntegral timeDiffFromUtc.getSeconds) utcTime
    secondsIntoDay = realToFrac (T.utctDayTime localTime) :: Double
    hourOfDay = floor secondsIntoDay `div` (3600 :: Int)

windowForHour :: Int -> APITypes.FRFSNudgeTimeWindow
windowForHour hourOfDay
  | hourOfDay >= 5 && hourOfDay < 12 = APITypes.MORNING
  | hourOfDay >= 12 && hourOfDay < 17 = APITypes.AFTERNOON
  | hourOfDay >= 17 && hourOfDay < 21 = APITypes.EVENING
  | otherwise = APITypes.NIGHT

oppositeWindow :: APITypes.FRFSNudgeTimeWindow -> APITypes.FRFSNudgeTimeWindow
oppositeWindow APITypes.MORNING = APITypes.EVENING
oppositeWindow APITypes.AFTERNOON = APITypes.EVENING
oppositeWindow APITypes.EVENING = APITypes.MORNING
oppositeWindow APITypes.NIGHT = APITypes.MORNING
