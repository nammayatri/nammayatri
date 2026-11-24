-- | Shared pieces of the shuttle subscription: when a term's slot departs on a day, the change cutoff
-- (reschedule, skip, cancel close 5 hours before departure, which is also when the day's ticket is issued),
-- and whether a day already has a live ticket.
module SharedLogic.ShuttlePass
  ( changeCutoff,
    cityTimeDiff,
    localDay,
    slotDepartureOn,
    issueTimeOn,
    hasLiveTicket,
    liveTickets,
    otpLeadTime,
    newBoardingOtp,
  )
where

import BecknV2.FRFS.Utils (frfsVehicleCategoryToBecknVehicleCategory)
import qualified Data.Time as DT
import qualified Domain.Types.FRFSTicketBooking as DBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DStatus
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Pass as DPass
import qualified Domain.Types.PurchasedPassPayment as DPPP
import qualified Domain.Types.RiderConfig as DRC
import Environment (Flow)
import qualified Environment
import Kernel.Prelude
import Kernel.Types.Error (GenericError (..))
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified SharedLogic.External.Nandi.Types as NandiTypes
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.CachedQueries.Pass as CQPass
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.FRFSTicketBookingExtra as QBookingExtra
import System.Random (randomRIO)
import Text.Printf (printf)

-- | Reschedule, skip and cancel close this long before the slot departs; the day's ticket is issued then.
changeCutoff :: NominalDiffTime
changeCutoff = 5 * 3600

-- | The boarding OTP, with the driver and vehicle, is sent to the rider this long before departure.
otpLeadTime :: NominalDiffTime
otpLeadTime = 3600

-- | A 4-digit code, zero-padded (stored in frfs_ticket_booking.boarding_otp, character(4)).
newBoardingOtp :: Flow Text
newBoardingOtp = do
  n <- liftIO $ randomRIO (0, 9999 :: Int)
  pure . toText $ (printf "%04d" n :: String)

cityTimeDiff :: Id DMOC.MerchantOperatingCity -> Flow Seconds
cityTimeDiff cityId = do
  mbRiderConfig <- getConfig (RiderConfigDimensions {merchantOperatingCityId = cityId.getId}) Nothing
  pure $ maybe (Seconds 19800) (\(config :: DRC.RiderConfig) -> config.timeDiffFromUtc) mbRiderConfig

localDay :: Seconds -> UTCTime -> DT.Day
localDay timeDiff t = DT.utctDay $ addUTCTime (fromIntegral timeDiff.getSeconds) t

localMidnightUtc :: Seconds -> DT.Day -> UTCTime
localMidnightUtc timeDiff day = addUTCTime (negate $ fromIntegral timeDiff.getSeconds) (DT.UTCTime day 0)

-- | When the term's slot leaves the rider's pickup stop on that day. Nothing when the term has no slot or
-- the timetable does not list the pickup on the trip.
slotDepartureOn :: Seconds -> DPPP.PurchasedPassPayment -> DT.Day -> Flow (Maybe UTCTime)
slotDepartureOn timeDiff term day = case (term.slotTripId, term.sourceStopCode, term.passId) of
  (Just slotTripId, Just pickupStopCode, Just passId) -> do
    (pass :: DPass.Pass) <- CQPass.findById passId >>= fromMaybeM (InvalidRequest $ "Pass not found: " <> passId.getId)
    integratedBPPConfig <- SIBC.findIntegratedBPPConfig Nothing term.merchantOperatingCityId (frfsVehicleCategoryToBecknVehicleCategory pass.vehicleType) DIBC.MULTIMODAL
    mbTripInfo <- OTPRest.getNandiTripInfo integratedBPPConfig slotTripId
    pickupCodes <- OTPRest.getEquivalentStopCodes pickupStopCode integratedBPPConfig
    -- GTFS departure is seconds after local midnight of the service day (may exceed 24h for late trips).
    pure $ do
      tripInfo <- mbTripInfo
      stop <- find (\(s :: NandiTypes.StopSchedule) -> s.stopCode `elem` pickupCodes) tripInfo.schedule
      pure $ addUTCTime (fromIntegral stop.departureTime) (localMidnightUtc timeDiff day)
  _ -> pure Nothing

issueTimeOn :: UTCTime -> UTCTime
issueTimeOn departure = addUTCTime (negate changeCutoff) departure

-- | A ticket of this term for the departure that is still live (not failed, cancelled or moved by a
-- reschedule). Its startTime lies between the issue time and the departure, so a window around them is enough.
hasLiveTicket :: DPPP.PurchasedPassPayment -> UTCTime -> Flow Bool
hasLiveTicket term departure = not . null <$> liveTickets term departure

liveTickets :: DPPP.PurchasedPassPayment -> UTCTime -> Flow [DBooking.FRFSTicketBooking]
liveTickets term departure = do
  bookings <-
    QBookingExtra.findAllByOverrideAppliedEntityIdAndStartTimeRange
      term.id.getId
      (addUTCTime (negate $ changeCutoff + 3600) departure)
      (addUTCTime 3600 departure)
  pure $ filter (\(booking :: DBooking.FRFSTicketBooking) -> booking.status `notElem` deadStatuses) bookings
  where
    deadStatuses = [DStatus.FAILED, DStatus.CANCELLED, DStatus.COUNTER_CANCELLED, DStatus.RESCHEDULED]
