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
import qualified Domain.Action.UI.Search as Search
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBookingStatus
import qualified Domain.Types.RiderConfig as DRiderConfig
import Kernel.Prelude
import Kernel.Types.Id (getId)
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified SharedLogic.FRFSUtils as FRFSUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.FRFSTicketBookingExtra as QFRFSTicketBookingExtra
import qualified Tools.Metrics as Metrics

-- | RETURN_TRIP/REPEAT_TRIP are about encouraging a rider to *start* using the repeat-booking
-- flow, before any cart exists -- scoped to this same route (same fromStationCode/toStationCode
-- pair), one capped query tells "first time" from "repeating":
--   * exactly 1 confirmed booking on this route -> the rider's first time on it, suggest the
--     return leg (RETURN_TRIP).
--   * 2 or more -> repeating this route on their own, nudge them toward the repeat-booking flow
--     (REPEAT_TRIP).
-- BUY_PASS is a different question entirely -- not "how many times has this rider done this
-- route" (a frequency guess), but "did they just actually pay cash for a repeat via the cart."
-- bookingGroupId is only ever set by that cart checkout, so it's a direct fact, not an inference:
-- a booking that has one IS a repeat the rider already acted on, regardless of how many prior
-- standalone bookings (if any) exist on this route.
computeShuttleNudge ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Metrics.CoreMetrics m, HasShortDurationRetryCfg r c) =>
  DFRFSTicketBooking.FRFSTicketBooking ->
  m (Maybe APITypes.FRFSNudgeAPI)
computeShuttleNudge booking
  | booking.status /= DFRFSTicketBookingStatus.CONFIRMED = pure Nothing
  | booking.vehicleType /= Spec.BUS = pure Nothing
  | isJust booking.bookingGroupId =
    pure $
      Just $
        mkSameDirectionNudge
          (if booking.overrideType == Just DFRFSTicketBooking.PassOverride then APITypes.REPEAT_TRIP else APITypes.BUY_PASS)
          booking
  | otherwise = do
    -- Only ever need to tell "exactly one" from "two or more" -- capping the fetch keeps this
    -- cheap on the hot booking-status/booking-list paths that call it once per booking.
    sameRouteConfirmed <-
      QFRFSTicketBookingExtra.findAllConfirmedByRiderIdVehicleTypeAndRoute
        booking.riderId
        Spec.BUS
        booking.fromStationCode
        booking.toStationCode
        (Just 2)
    case length sameRouteConfirmed of
      1 -> do
        mbRiderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = getId booking.merchantOperatingCityId}) Nothing
        let timeDiffFromUtc = maybe (Seconds 19800) DRiderConfig.timeDiffFromUtc mbRiderConfig
        Just <$> mkReturnTripNudge booking timeDiffFromUtc
      n | n >= 2 -> pure $ Just (mkSameDirectionNudge APITypes.REPEAT_TRIP booking)
      _ -> pure Nothing

-- | Same direction as the booking itself (nothing to swap or normalize -- these are already this
-- booking's own station codes). No time-window hint: for REPEAT_TRIP the real matched time comes
-- from calling GET /frfs/booking/{bookingId}/repeatSlots on this same booking, not a coarse
-- guess; for BUY_PASS the real pass price/terms come from the pass-purchase flow itself.
mkSameDirectionNudge :: APITypes.FRFSNudgeType -> DFRFSTicketBooking.FRFSTicketBooking -> APITypes.FRFSNudgeAPI
mkSameDirectionNudge nudgeType booking =
  APITypes.FRFSNudgeAPI
    { nudgeType,
      fromStationCode = booking.fromStationCode,
      toStationCode = booking.toStationCode,
      timeWindowHint = Nothing
    }

-- | Swaps the booking's stations for the reverse direction, normalizing each through its parent
-- station first (same pattern as FRFSUtils.stationCodeForMatching) -- the alighting and boarding
-- platforms for opposite directions of the same stop can be different platform-level codes under
-- one logical station, and a raw swap of those codes would point at the wrong platform.
mkReturnTripNudge ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Metrics.CoreMetrics m, HasShortDurationRetryCfg r c) =>
  DFRFSTicketBooking.FRFSTicketBooking ->
  Seconds ->
  m APITypes.FRFSNudgeAPI
mkReturnTripNudge booking timeDiffFromUtc = do
  integratedBPPConfig <- SIBC.findIntegratedBPPConfigById booking.integratedBppConfigId
  fromStationCode <- normalizeStationCode integratedBPPConfig booking.toStationCode
  toStationCode <- normalizeStationCode integratedBPPConfig booking.fromStationCode
  pure
    APITypes.FRFSNudgeAPI
      { nudgeType = APITypes.RETURN_TRIP,
        fromStationCode,
        toStationCode,
        timeWindowHint = oppositeWindow . bucketTimeWindow timeDiffFromUtc <$> booking.startTime
      }
  where
    normalizeStationCode integratedBPPConfig' stopCode = fromMaybe stopCode <$> FRFSUtils.stationCodeForMatching integratedBPPConfig' stopCode

-- | getLocalHourOfDay returns an hour in 1-24 (todHour + 1); windowForHour's bounds assume the
-- usual 0-23 clock hour, hence the -1.
bucketTimeWindow :: Seconds -> UTCTime -> APITypes.FRFSNudgeTimeWindow
bucketTimeWindow timeDiffFromUtc utcTime = windowForHour (Search.getLocalHourOfDay timeDiffFromUtc utcTime - 1)

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
